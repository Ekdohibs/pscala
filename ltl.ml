open Ltl_ast
open Common
	   
let graph = ref LMap.empty
let generate instr =
  let l = Label.fresh () in
  graph := LMap.add l instr !graph;
  l
	   
let get_color colors r =
  Rmap.find r colors

let word_size = 8
			
let tmp1 = Register.R10
let tmp2 = Register.R11
let write colors r l =
  match get_color colors r with
  | Coloring.Reg r -> r, l
  | Coloring.Spilled n ->
	 tmp1, generate (Ebinary (Xmov, Coloring.Reg tmp1,
							        Coloring.Spilled n, l))

let read_tmp tmp colors r f =
  match get_color colors r with
  | Coloring.Reg r -> f r
  | Coloring.Spilled n ->
	 Ebinary (Xmov, Coloring.Spilled n, Coloring.Reg tmp,
			  generate (f tmp))
let read1 = read_tmp tmp1
			 
let read2 colors r1 r2 f =
  read_tmp tmp1 colors r1 (fun r1 -> read_tmp tmp2 colors r2 (f r1))
			 
let instr colors framesize = function
  | Ertl_ast.Eint (n, r, l) ->
	 let c = get_color colors r in
	 if Ertl.is32op n then
	   Eint (Int64.to_int32 n, c, l)
	 else
	   let r, l = write colors r l in
	   Eint64 (n, r, l)
  | Ertl_ast.Estring (s, r, l) ->
	 Estring (s, get_color colors r, l)
  | Ertl_ast.Eunit (r, l) -> Egoto l
  | Ertl_ast.Egetfield (r1, n, r2, l) ->
	 let r2, l = write colors r2 l in
	 read1 colors r1 (fun r1 -> Egetfield (r1, n, r2, l))
  | Ertl_ast.Esetfield (r1, n, r2, l) ->
	 read2 colors r1 r2 (fun r1 r2 -> Esetfield (r1, n, r2, l))
  | Ertl_ast.Ecall (s, _, l) -> Ecall (s, l)
  | Ertl_ast.Ecallmethod (offset, _, l) -> Ecallmethod (offset, l)
  | Ertl_ast.Esetheader (s, r, l) ->
	 read1 colors r (fun r -> Esetheader (s, r, l))
  | Ertl_ast.Eunary (op, r, l) ->
	 Eunary (op, get_color colors r, l)
  | Ertl_ast.Ebinary (op, r1, r2, l) ->
	 (match op, get_color colors r1, get_color colors r2 with
	  | Xmov, o1, o2 when o1 = o2 -> Egoto l
	  | _, (Coloring.Spilled _ as o1), (Coloring.Spilled _ as o2)
	  | Xmul, o1, (Coloring.Spilled _ as o2) ->
		 read1 colors r2 (fun r2 ->
		   Ebinary (op, o1, Coloring.Reg r2, generate (
		   Ebinary (Xmov, Coloring.Reg r2, o2, l))))
	  | _, o1, o2 -> Ebinary (op, o1, o2, l))
  | Ertl_ast.Ecqto l -> Ecqto l
  | Ertl_ast.Egoto l -> Egoto l
  | Ertl_ast.Eubranch (u, r, l1, l2) ->
	 Eubranch (u, get_color colors r, l1, l2)
  | Ertl_ast.Ebbranch (b, r1, r2, l1, l2) ->
	 (match get_color colors r1, get_color colors r2 with
	  | (Coloring.Spilled _ as o1), Coloring.Spilled _ ->
		 read1 colors r2 (fun r2 ->
		   Ebbranch (b, o1, Coloring.Reg r2, l1, l2))
	  | o1, o2 -> Ebbranch (b, o1, o2, l1, l2)
	 )
  | Ertl_ast.Euset (u, r1, r2, l) ->
	 Euset (u, get_color colors r1, get_color colors r2, l)
  | Ertl_ast.Ebset (b, r1, r2, r3, l) ->
 	(match get_color colors r1, get_color colors r2 with
	 | (Coloring.Spilled _ as o1), Coloring.Spilled _ ->
		read1 colors r2 (fun r2 ->
		  Ebset (b, o1, Coloring.Reg r2, get_color colors r3, l))
	  | o1, o2 -> Ebset (b, o1, o2, get_color colors r3, l)
	)
  | Ertl_ast.Ealloc_frame l ->
	 if framesize = 0 then
	   Egoto l
	 else
	   Eunary (Xaddi (Int32.of_int (-word_size * framesize)),
			   Coloring.Reg Register.Rsp, l)
  | Ertl_ast.Edelete_frame l ->
	 if framesize = 0 then
	   Egoto l
	 else
	   Eunary (Xaddi (Int32.of_int (word_size * framesize)),
			   Coloring.Reg Register.Rsp, l)
  | Ertl_ast.Eget_param (n, r, l) ->
	 let r, l = write colors r l in
	 let n = framesize + 1 + n in
	 Ebinary (Xmov, Coloring.Spilled n, Coloring.Reg r, l)
  | Ertl_ast.Epush_param (r, l) ->
	 Epush_param (get_color colors r, l)
  | Ertl_ast.Estack_free (n, l) ->
	 if n = 0 then Egoto l else
	   Eunary (Xaddi (Int32.of_int (word_size * n)),
			   Coloring.Reg Register.Rsp, l)
  | Ertl_ast.Ereturn -> Ereturn

module Rset = Set.Make(Register)						  
let func f =
  let lv = Liveliness.analyse f.Ertl_ast.fun_body in
  let ig = Interference.make lv in
  Debug.debug "inter done (%d vertices, %d prefs, %d interf)@."
   (Rmap.cardinal ig)
   (Rmap.fold (fun _ arc s -> s + Rset.cardinal arc.Interference.prefs) ig 0)
   (Rmap.fold (fun _ arc s -> s + Rset.cardinal arc.Interference.interf) ig 0)
  ;
  let coloring, nlocals = Coloring.find_coloring ig in
  graph := LMap.empty;
  LMap.iter (fun l i ->
			 let i = instr coloring nlocals i in
			 graph := LMap.add l i !graph) f.Ertl_ast.fun_body;
  { fun_name = f.Ertl_ast.fun_name;
	fun_entry = f.Ertl_ast.fun_entry;
	fun_body = !graph
  }

let program p = 
 { prog_functions = List.map func p.Ertl_ast.prog_functions;
   prog_class_descrs = p.Ertl_ast.prog_class_descrs
 }


let next_labels = function
  | Eint (_, _, l) | Eint64(_, _, l) | Estring (_, _, l)
  | Egetfield (_, _, _, l) | Esetfield (_, _, _, l)
  | Ecall (_, l) | Ecallmethod (_, l)
  | Esetheader (_, _, l) | Eunary (_, _, l)
  | Ebinary (_, _, _, l) | Ecqto l | Egoto l
  | Euset (_, _, _, l) | Ebset (_, _, _, _, l)
  | Epush_param (_, l) -> [l]
  | Eubranch (_, _, l1, l2) | Ebbranch (_, _, _, l1, l2) -> [l1; l2]
  | Ereturn -> []

let print_reg ff r = Format.fprintf ff "%s" (Register.reg_to_string r)

let print_color ff = function
  | Coloring.Reg r -> Format.fprintf ff "%s" (Register.reg_to_string r)
  | Coloring.Spilled n -> Format.fprintf ff "%d(%%rsp)" (word_size * n)
				 
let print_instr ff i =
  (match i with
   | Eint (n, r, l) ->
	  Format.fprintf ff "%a <- %ld" print_color r n
   | Eint64 (n, r, l) ->
	  Format.fprintf ff "%a <- %Ld" print_reg r n
   | Estring (s, r, l) ->
	  Format.fprintf ff "%a <- \"%s\"" print_color r s
   | Egetfield (r1, n, r2, l) ->
	  Format.fprintf ff "%a <- %a[%d]"
					 print_reg r2 print_reg r1 n
   | Esetfield (r1, n, r2, l) ->
	  Format.fprintf ff "%a[%d] <- %a"
					 print_reg r1 n print_reg r2
   | Ecall (f, l) ->
	  Format.fprintf ff "call %s" f
   | Ecallmethod (offset, l) ->
	  Format.fprintf ff "callm %d" offset
   | Esetheader(s, r, l) ->
	  Format.fprintf ff "set_header %a %s" print_reg r s
   | Eunary (op, r, l) ->
	  Format.fprintf ff "%a %a" print_xunary op print_color r
   | Ebinary (op, r1, r2, l) ->
	  Format.fprintf ff "%a %a %a" print_xbinary op
					 print_color r1 print_color r2
   | Ecqto l -> Format.fprintf ff "cqto"
   | Egoto l -> ()
   | Eubranch (b, r, l1, l2) ->
	  Format.fprintf ff "j%a" print_ubranch
					 (b, (fun ff -> print_color ff r))
   | Ebbranch (b, r1, r2, l1, l2) ->
	  Format.fprintf ff "j%a %a %a" print_bbranch b
					print_color r1 print_color r2
   | Euset (b, r1, r2, l) ->
	  Format.fprintf ff "set%a %a" print_ubranch
					 (b, (fun ff -> print_color ff r1))
					 print_color r2
   | Ebset (b, r1, r2, r3, l) ->
	  Format.fprintf ff "set%a %a %a %a" print_bbranch b
				     print_color r1 print_color r2 print_color r3
   | Epush_param (r, l) ->
	  Format.fprintf ff "push %a" print_color r
   | Ereturn ->
	  Format.fprintf ff "ret"
  );
  Format.pp_print_tab ff ();
  Format.fprintf ff "\t--> ";
  print_list ff Label.print ", " (next_labels i)

let rec print_graph ff graph seen i =
  if Hashtbl.mem seen i then () else begin
	Hashtbl.add seen i ();
	try
	  let instr = LMap.find i graph in
	  Format.fprintf ff "%a: %a@\n" Label.print i print_instr instr;
	  List.iter (print_graph ff graph seen) (next_labels instr)
	with
	  Not_found -> ()
  end

let print_func ff f =
  Format.fprintf ff "%s@\n" f.fun_name;
  Format.fprintf ff "Entry: %a@[<v 2>@\n" Label.print f.fun_entry;
  print_graph ff f.fun_body (Hashtbl.create 17) f.fun_entry;
  Format.fprintf ff "@]@."

let print_program ff p =
  print_list ff print_func "@\n" p.prog_functions;
  Format.fprintf ff "Class descriptors:@\n";
  print_list ff (fun ff (class_name, parent_name, data) ->
				 Format.fprintf ff "%s (inherits %s)@[<v 2>@\n"
								class_name parent_name;
				 print_list ff (fun ff -> Format.fprintf ff "%s") "@\n" data;
				 Format.fprintf ff "@]")
			 "@\n@\n" p.prog_class_descrs;
  Format.fprintf ff "@."
   
