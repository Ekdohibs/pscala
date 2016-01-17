open Rtl_ast
open Common
	   
let graph = ref LMap.empty
let generate instr =
  let l = Label.fresh () in
  graph := LMap.add l instr !graph;
  l

let loop f =
  let l = Label.fresh () in
  let entry = f l in
  graph := LMap.add l (Egoto entry) !graph;
  entry
	
let move r1 r2 l = generate (Ebinary (Xmov, r1, r2, l))
							
let get_local_register i locals =
  try Hashtbl.find locals i with
	Not_found ->
	let r = Register.fresh () in
	Hashtbl.add locals i r;
	r

let is_utest = function
  | Is_ast.Xeqi _ | Is_ast.Xnei _ | Is_ast.Xlti _
  | Is_ast.Xlei _ | Is_ast.Xgti _ | Is_ast.Xgei _ -> true
  | _ -> false

let utest_to_ubranch = function
  | Is_ast.Xeqi n -> Ujeqi n | Is_ast.Xnei n -> Ujnei n
  | Is_ast.Xlti n -> Ujlti n | Is_ast.Xlei n -> Ujlei n
  | Is_ast.Xgti n -> Ujgti n | Is_ast.Xgei n -> Ujgei n
  | _ -> assert false

let is_btest = function
  | Is_ast.Xeq | Is_ast.Xne | Is_ast.Xlt
  | Is_ast.Xle | Is_ast.Xgt | Is_ast.Xge -> true
  | _ -> false

let btest_to_bbranch = function
  | Is_ast.Xeq -> Bjeq | Is_ast.Xne -> Bjne
  | Is_ast.Xlt -> Bjlt | Is_ast.Xle -> Bjle
  | Is_ast.Xgt -> Bjgt | Is_ast.Xge -> Bjge
  | _ -> assert false

				
let rec condition e truel falsel ex locals = match e.Is_ast.is_expr with
  | Is_ast.Eand (e1, e2) ->
	 condition e1 (condition e2 truel falsel ex locals) falsel ex locals
  | Is_ast.Eor (e1, e2) ->
	 condition e1 truel (condition e2 truel falsel ex locals) ex locals
  | Is_ast.Eunary (Is_ast.Xnot, e) ->
	 condition e falsel truel ex locals
  | Is_ast.Eunary (op, e) when is_utest op ->
	 let r = Register.fresh () in
	 expr r e ex locals (
	 generate (Eubranch (utest_to_ubranch op, r, truel, falsel)))
  | Is_ast.Ebinary (op, e1, e2) when is_btest op ->
	 let r1 = Register.fresh () in
	 let r2 = Register.fresh () in
	 expr r1 e1 ex locals (
	 expr r2 e2 ex locals (
	 generate (Ebbranch (btest_to_bbranch op, r1, r2, truel, falsel))))
   | _ -> let r = Register.fresh () in
		 expr r e ex locals (
		 generate (Eubranch (Ujz, r, falsel, truel)))
  
and expr destr e ex locals destl = match e.Is_ast.is_expr with
  | Is_ast.Eint n -> generate (Eint (n, destr, destl))
  | Is_ast.Estring s -> generate (Estring (s, destr, destl))
  | Is_ast.Eunit -> generate (Eunit (destr, destl))
  | Is_ast.Egetlocal i ->
	 move (get_local_register i locals) destr destl
  | Is_ast.Esetlocal (i, e) ->
	 let r = Register.fresh () in
	 expr r e ex locals (
	 move r (get_local_register i locals) (generate (
	 Eunit (destr, destl))))
  | Is_ast.Egetfield (e, n) ->
	 let r = Register.fresh () in
	 expr r e ex locals (
	 generate (Egetfield (r, n, destr, destl)))
  | Is_ast.Esetfield (e1, n, e2) ->
	 let r1 = Register.fresh () in
	 let r2 = Register.fresh () in
	 expr r1 e1 ex locals (
	 expr r2 e2 ex locals (
	 generate (Esetfield (r1, n, r2,
	 generate (Eunit (destr, destl))))))
  | Is_ast.Ecall (f, _, l) ->
	 let regs = List.map (fun _ -> Register.fresh ()) l in
	 List.fold_right (fun (e, r) l -> expr r e ex locals l)
					 (List.combine l regs)
					 (generate (Ecall (destr, f, regs, destl)))
  | Is_ast.Ecallmethod (offset, _, l) ->
	 let regs = List.map (fun _ -> Register.fresh ()) l in
	 List.fold_right (fun (e, r) l -> expr r e ex locals l)
					 (List.combine l regs)
					 (generate (Ecallmethod (destr, offset, regs, destl)))
  | Is_ast.Eallocbloc (s, _, i) -> generate (Eallocbloc (s, i, destr, destl))
  | Is_ast.Ereturn e ->
	 let (retr, retl) = ex in
	 expr retr e ex locals retl
  | Is_ast.Eprintint e ->
	 let r = Register.fresh () in
	 expr r e ex locals (generate (Eprintint (r,
	 generate (Eunit (destr, destl)))))
  | Is_ast.Eprintstring e ->
	 let r = Register.fresh () in
	 expr r e ex locals (generate (Eprintstring (r,
	 generate (Eunit (destr, destl)))))
  | Is_ast.Ebloc b ->
	 snd (List.fold_right (fun e (r, l) ->
						   Register.fresh (), expr r e ex locals l)
						  b (destr, destl))
  | Is_ast.Eif (e1, e2, e3) ->
	 condition e1
			   (expr destr e2 ex locals destl)
			   (expr destr e3 ex locals destl)
			   ex locals
  | Is_ast.Ewhile (e1, e2) ->
	 loop (fun l -> condition e1 (expr destr e2 ex locals l)
							  (generate (Eunit (destr, destl)))
							  ex locals)
  | Is_ast.Eand (e1, e2) ->
	 condition e1
			   (expr destr e2 ex locals destl)
			   (generate (Eint (0L, destr, destl))) ex locals
  | Is_ast.Eor (e1, e2) ->
	 condition e1
			   (generate (Eint (1L, destr, destl)))
			   (expr destr e2 ex locals destl) ex locals

  | Is_ast.Eunary (op, e) ->
	 if is_utest op then
	   let r = Register.fresh () in
	   expr r e ex locals (
	   generate (Euset (utest_to_ubranch op, r, destr, destl)))
	 else
		let o = match op with 
		  | Is_ast.Xneg -> Xneg | Is_ast.Xnot -> Xnot
		  | Is_ast.Xaddi n -> Xaddi n | Is_ast.Xmuli n -> Xmuli n
		  | Is_ast.Xdivi n -> Xdivi n | Is_ast.Xmodi n -> Xmodi n
		  | Is_ast.Xsli n -> Xsli n | Is_ast.Xasri n -> Xasri n
		  | Is_ast.Xlsri n -> Xlsri n
		  | _ -> assert false
		in
		expr destr e ex locals (
		generate (Eunary (o, destr, destl)))
  | Is_ast.Ebinary (Is_ast.Xadd | Is_ast.Xmul as op, e1, e2) ->
	 let o = match op with
	   | Is_ast.Xadd -> Xcadd | Is_ast.Xmul -> Xcmul
	   | _ -> assert false in
	 let r1 = Register.fresh () in
	 let r2 = Register.fresh () in
	 expr r1 e1 ex locals (
	 expr r2 e2 ex locals (
	 generate (Ebinary3 (o, r1, r2, destr, destl))))
  | Is_ast.Ebinary (op, e1, e2) ->
	 if is_btest op then
	   	let r1 = Register.fresh () in
		let r2 = Register.fresh () in
		expr r1 e1 ex locals (
		expr r2 e2 ex locals (
		generate (Ebset (btest_to_bbranch op, r1, r2, destr, destl))))
	 else
		let o = match op with
		  | Is_ast.Xadd -> Xadd | Is_ast.Xsub -> Xsub
		  | Is_ast.Xrsub -> Xsub | Is_ast.Xmul -> Xmul
		  | Is_ast.Xdiv -> Xdiv | Is_ast.Xmod -> Xmod
		  | Is_ast.Xsl -> Xsl | Is_ast.Xasr -> Xasr
		  | Is_ast.Xlsr -> Xlsr
		  | _ -> assert false
		in
		let r1 = Register.fresh () in
		let r2 = Register.fresh () in
		expr r1 e1 ex locals (
		expr r2 e2 ex locals (
        if op = Is_ast.Xrsub then
		  move r2 destr (generate (Ebinary (o, r1, destr, destl)))
		else
		  move r1 destr (generate (Ebinary (o, r2, destr, destl)))
		))

let func f =
  graph := LMap.empty;
  let locals = Hashtbl.create 17 in
  let params = List.map (fun i -> get_local_register i locals)
						(List.range 0 f.Is_ast.fun_params) in
  let result = Register.fresh () in
  let ex = Label.fresh () in
  let entry = expr result f.Is_ast.fun_body (result, ex) locals ex in
  {
	fun_name = f.Is_ast.fun_name;
	fun_params = params;
	fun_result = result;
	fun_entry = entry;
	fun_exit = ex;
	fun_body = !graph;
	fun_has_value = f.Is_ast.fun_has_value
  }

let program prog = {
  prog_functions = List.map func prog.Is_ast.prog_functions;
  prog_class_descrs = prog.Is_ast.prog_class_descrs
}

let print_xunary ff = function
  | Xneg -> Format.fprintf ff "neg"
  | Xnot -> Format.fprintf ff "not"
  | Xaddi n -> Format.fprintf ff "addi $%Ld" n
  | Xmuli n -> Format.fprintf ff "muli $%Ld" n
  | Xdivi n -> Format.fprintf ff "divi $%Ld" n
  | Xmodi n -> Format.fprintf ff "modi $%Ld" n
  | Xsli n -> Format.fprintf ff "sli $%d" n
  | Xasri n -> Format.fprintf ff "asri $%d" n
  | Xlsri n -> Format.fprintf ff "lsri $%d" n

let print_xbinary ff b =
  Format.fprintf ff "%s" (match b with
	| Xadd -> "add" | Xsub -> "sub" | Xmul -> "imul"
	| Xdiv -> "div" | Xmod -> "mod" | Xsl -> "sl"
	| Xasr -> "asr" | Xlsr -> "lsr" | Xmov -> "mov")

let print_xcbinary ff b =
  Format.fprintf ff "%s" (match b with
	| Xcadd -> "add" | Xcmul -> "imul")
				 
let print_ubranch ff (b, f) = match b with
  | Ujeqi n -> Format.fprintf ff "eqi %t $%Ld" f n
  | Ujnei n -> Format.fprintf ff "nei %t $%Ld" f n
  | Ujlti n -> Format.fprintf ff "lti %t $%Ld" f n
  | Ujlei n -> Format.fprintf ff "lei %t $%Ld" f n
  | Ujgti n -> Format.fprintf ff "gti %t $%Ld" f n
  | Ujgei n -> Format.fprintf ff "gei %t $%Ld" f n
  | Ujz -> Format.fprintf ff "z %t" f
  | Ujnz -> Format.fprintf ff "nz %t" f

let print_bbranch ff b =
  Format.fprintf ff "%s" (match b with
  | Bjeq -> "eq" | Bjne -> "ne" | Bjlt -> "lt"
  | Bjle -> "le" | Bjgt -> "gt" | Bjge -> "ge")

let next_labels = function
  | Eint (_, _, l) | Estring (_, _, l) | Eunit (_, l)
  | Egetfield (_, _, _, l) | Esetfield (_, _, _, l)
  | Ecall (_, _, _, l) | Ecallmethod (_, _, _, l)
  | Eallocbloc (_, _, _, l) | Eunary (_, _, l)
  | Ebinary (_, _, _, l) | Ebinary3 (_, _, _, _, l) | Eprintint (_, l)
  | Eprintstring (_, l) | Egoto l
  | Euset (_, _, _, l) | Ebset (_, _, _, _, l) -> [l]
  | Eubranch (_, _, l1, l2) | Ebbranch (_, _, _, l1, l2) -> [l1; l2]
				 
let print_instr ff i =
  (match i with
   | Eint (n, r, l) ->
	  Format.fprintf ff "%a <- %Ld" Register.print r n
   | Estring (s, r, l) ->
	  Format.fprintf ff "%a <- \"%s\"" Register.print r s
   | Eunit (r, l) ->
	  Format.fprintf ff "unit %a" Register.print r
   | Egetfield (r1, n, r2, l) ->
	  Format.fprintf ff "%a <- %a[%d]"
					Register.print r2 Register.print r1 n
   | Esetfield (r1, n, r2, l) ->
	  Format.fprintf ff "%a[%d] <- %a"
					 Register.print r1 n Register.print r2
   | Ecall (r, f, args, l) ->
	  Format.fprintf ff "%a <- %s(" Register.print r f;
	  print_list ff Register.print ", " args;
	  Format.fprintf ff ")"
   | Ecallmethod (r, offset, args, l) ->
	  Format.fprintf ff "%a <- %d(" Register.print r offset;
	  print_list ff Register.print ", "args;
	  Format.fprintf ff ")"
   | Eallocbloc(s, n, r, l) ->
	  Format.fprintf ff "%a <- make_bloc(%s, %d)"
					 Register.print r s n
   | Eunary (op, r, l) ->
	  Format.fprintf ff "%a %a" print_xunary op Register.print r
   | Ebinary (op, r1, r2, l) ->
	  Format.fprintf ff "%a %a %a" print_xbinary op
					 Register.print r1 Register.print r2
   | Ebinary3 (op, r1, r2, r3, l) ->
	  Format.fprintf ff "%a %a %a %a" print_xcbinary op
				 Register.print r1 Register.print r2 Register.print r3
   | Eprintint (r, l) ->
	  Format.fprintf ff "print_int %a" Register.print r
   | Eprintstring (r, l) ->
	  Format.fprintf ff "print_string %a" Register.print r
   | Egoto l -> ()
   | Eubranch (b, r, l1, l2) ->
	  Format.fprintf ff "j%a" print_ubranch
					 (b, (fun ff -> Register.print ff r))
   | Ebbranch (b, r1, r2, l1, l2) ->
	  Format.fprintf ff "j%a %a %a" print_bbranch b
					 Register.print r1 Register.print r2
   | Euset (b, r1, r2, l) ->
	  Format.fprintf ff "set%a %a" print_ubranch
					 (b, (fun ff -> Register.print ff r1))
					 Register.print r2
   | Ebset (b, r1, r2, r3, l) ->
	  Format.fprintf ff "set%a %a %a %a" print_bbranch b
					 Register.print r1 Register.print r2 Register.print r3
  );
  Format.pp_print_tab ff ();
  Format.fprintf ff "\t--> ";
  print_list ff Label.print ", " (next_labels i);
  Format.fprintf ff "@\n"

let rec print_graph ff graph seen i =
  if Hashtbl.mem seen i then () else begin
	Hashtbl.add seen i ();
	try
	  let instr = LMap.find i graph in
	  Format.fprintf ff "%a: %a" Label.print i print_instr instr;
	  List.iter (print_graph ff graph seen) (next_labels instr)
	with
	  Not_found -> ()
  end

let print_func ff f =
  Format.fprintf ff "%a %s(%t)@\n" Register.print f.fun_result
				 f.fun_name
				 (fun ff -> print_list ff Register.print ", " f.fun_params);
  Format.fprintf ff "Entry: %a@\n" Label.print f.fun_entry;
  Format.fprintf ff "Exit: %a@[<v 2>@\n" Label.print f.fun_exit;
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
