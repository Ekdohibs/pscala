open Ertl_ast
open Common
	   
let word_size = 8

let graph = ref LMap.empty
let generate instr =
  let l = Label.fresh () in
  graph := LMap.add l instr !graph;
  l

let move r1 r2 l = generate (Ebinary (Xmov, r1, r2, l))
let is32op n = Int64.of_int32 Int32.min_int <= n &&
				 n <= Int64.of_int32 Int32.max_int

let ubranch_ok u = Rtl_ast.(match u with
  | Ujz | Ujnz -> true
  | Ujeqi n | Ujnei n | Ujlti n | Ujlei n | Ujgti n | Ujgei n ->
	 is32op n)

let ubranch_to_32 = function
  | Rtl_ast.Ujeqi n -> Ujeqi (Int64.to_int32 n)
  | Rtl_ast.Ujnei n -> Ujnei (Int64.to_int32 n)
  | Rtl_ast.Ujlti n -> Ujlti (Int64.to_int32 n)
  | Rtl_ast.Ujlei n -> Ujlei (Int64.to_int32 n)
  | Rtl_ast.Ujgti n -> Ujgti (Int64.to_int32 n)
  | Rtl_ast.Ujgei n -> Ujgei (Int64.to_int32 n)
  | Rtl_ast.Ujz -> Ujz
  | Rtl_ast.Ujnz -> Ujnz

let bbranch_to_32 = function
  | Rtl_ast.Bjeq -> Bjeq
  | Rtl_ast.Bjne -> Bjne
  | Rtl_ast.Bjlt -> Bjlt
  | Rtl_ast.Bjle -> Bjle
  | Rtl_ast.Bjgt -> Bjgt
  | Rtl_ast.Bjge -> Bjge
					  
let ubranch_to_bbranch u = match u with
  | Rtl_ast.Ujeqi n -> Bjeq, n
  | Rtl_ast.Ujnei n -> Bjne, n
  | Rtl_ast.Ujlti n -> Bjlt, n
  | Rtl_ast.Ujlei n -> Bjle, n
  | Rtl_ast.Ujgti n -> Bjgt, n
  | Rtl_ast.Ujgei n -> Bjge, n
  | _ -> assert false

let param_registers params =
  let rec assoc params registers = match params, registers with
	  [], _ -> [], []
	| _, [] -> [], params
	| p :: ps, r :: rs ->
	   let regs, stack = assoc ps rs in
	   (p, r) :: regs, stack
  in
  assoc params Register.parameters
				
let instr = function
  | Rtl_ast.Eint (n, r, l) -> Eint (n, r, l)
  | Rtl_ast.Estring (s, r, l) -> Estring (s, r, l)
  | Rtl_ast.Eunit (r, l) -> Eunit (r, l)
  | Rtl_ast.Egetfield (r1, f, r2, l) -> Egetfield (r1, f, r2, l)
  | Rtl_ast.Esetfield (r1, f, r2, l) -> Esetfield (r1, f, r2, l)
  | Rtl_ast.Ecall (r, f, params, l) ->
	 let regs, stack = param_registers params in
	 Egoto (
	 List.fold_right (fun (param, register) l -> move param register l) regs (
	 List.fold_left (fun l reg -> generate (Epush_param (reg, l))) (
	  generate (Ecall (f, List.length params,
	  move Register.rax r (generate (Estack_free (List.length stack, l)))))
	 ) stack))	 
  | Rtl_ast.Ecallmethod (r, offset, params, l) ->
	 	 let regs, stack = param_registers params in
	 Egoto (
	 List.fold_right (fun (param, register) l -> move param register l) regs (
	 List.fold_left (fun l reg -> generate (Epush_param (reg, l))) (
	  generate (Ecallmethod (offset, List.length params,
	  move Register.rax r (generate (Estack_free (List.length stack, l)))))
	 ) stack))	 
  | Rtl_ast.Eallocbloc (s, n, r, l) ->
	 Eint (Int64.of_int (word_size * n), Register.rdi, generate (
	 Ecall ("malloc", 1, generate (
	 Esetheader (s, Register.rax,
	 move Register.rax r l)))))
  | Rtl_ast.Eunary (Rtl_ast.Xneg, r, l) -> Eunary (Xneg, r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xnot, r, l) -> Eunary (Xnot, r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xaddi n, r, l) when is32op n ->
	 Eunary (Xaddi (Int64.to_int32 n), r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xaddi n, r, l) ->
	 let r1 = Register.fresh () in
	 Eint (n, r1, generate (
	 Ebinary (Xadd, r1, r, l)))
  | Rtl_ast.Eunary (Rtl_ast.Xmuli n, r, l) when is32op n ->
	 Eunary (Xmuli (Int64.to_int32 n), r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xmuli n, r, l) ->
	 let r1 = Register.fresh () in
	 Eint (n, r1, generate (
	 Ebinary (Xmul, r1, r, l)))
  | Rtl_ast.Eunary (Rtl_ast.Xdivi n, r, l) when is32op n ->
	 Ebinary (Xmov, r, Register.rax, generate (
	 Ecqto (generate (
	 Eunary (Xdivi (Int64.to_int32 n), Register.rax,
	 move Register.rax r l)))))
  | Rtl_ast.Eunary (Rtl_ast.Xdivi n, r, l) ->
	 let r1 = Register.fresh () in
	 Eint (n, r1,
	 move r Register.rax (generate (
	 Ecqto (generate (
	 Ebinary (Xdiv, r1, Register.rax,
	 move Register.rax r l))))))
  | Rtl_ast.Eunary (Rtl_ast.Xmodi n, r, l) when is32op n ->
	 Ebinary (Xmov, r, Register.rax, generate (
	 Ecqto (generate (
	 Eunary (Xdivi (Int64.to_int32 n), Register.rax,
	 move Register.rdx r l)))))
  | Rtl_ast.Eunary (Rtl_ast.Xmodi n, r, l) ->
	 let r1 = Register.fresh () in
	 Eint (n, r1,
	 move r Register.rax (generate (
	 Ecqto (generate (
	 Ebinary (Xdiv, r1, Register.rax,
	 move Register.rdx r l))))))
  | Rtl_ast.Eunary (Rtl_ast.Xsli n, r, l) -> Eunary (Xsli n, r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xasri n, r, l) -> Eunary (Xasri n, r, l)
  | Rtl_ast.Eunary (Rtl_ast.Xlsri n, r, l) -> Eunary (Xlsri n, r, l)
  | Rtl_ast.Ebinary (Rtl_ast.Xadd, r1, r2, l) ->
	 Ebinary (Xadd, r1, r2, l)
  | Rtl_ast.Ebinary (Rtl_ast.Xsub, r1, r2, l) ->
	 Ebinary (Xsub, r1, r2, l)
  | Rtl_ast.Ebinary (Rtl_ast.Xmul, r1, r2, l) ->
	 Ebinary (Xmul, r1, r2, l)
  | Rtl_ast.Ebinary (Rtl_ast.Xdiv, r1, r2, l) ->
	 Ebinary (Xmov, r2, Register.rax, (generate (
	 Ecqto (generate (
	 Ebinary (Xdiv, r1, Register.rax,
	 move Register.rax r2 l))))))
  | Rtl_ast.Ebinary (Rtl_ast.Xmod, r1, r2, l) ->
	 Ebinary (Xmov, r2, Register.rax, (generate (
	 Ecqto (generate (
	 Ebinary (Xdiv, r1, Register.rax,
	 move Register.rdx r2 l))))))
  | Rtl_ast.Ebinary (Rtl_ast.Xsl, r1, r2, l) ->
	 Ebinary (Xmov, r1, Register.rcx, (generate (
	 Ebinary (Xsl, Register.rcx, r2, l))))
  | Rtl_ast.Ebinary (Rtl_ast.Xasr, r1, r2, l) ->
	 Ebinary (Xmov, r1, Register.rcx, (generate (
	 Ebinary (Xasr, Register.rcx, r2, l))))
  | Rtl_ast.Ebinary (Rtl_ast.Xlsr, r1, r2, l) ->
	 Ebinary (Xmov, r1, Register.rcx, (generate (
	 Ebinary (Xlsr, Register.rcx, r2, l))))
  | Rtl_ast.Ebinary (Rtl_ast.Xmov, r1, r2, l) ->
	 Ebinary (Xmov, r1, r2, l)
  | Rtl_ast.Ebinary3 (Rtl_ast.Xcadd, r1, r2, r3, l) ->
	 Ebinary3 (Xcadd, r1, r2, r3, l)
  | Rtl_ast.Ebinary3 (Rtl_ast.Xcmul, r1, r2, r3, l) ->
	 Ebinary3 (Xcmul, r1, r2, r3, l)
  | Rtl_ast.Eprintint (r, l) ->
	 Ebinary (Xmov, r, Register.rsi, (generate (
	 Estring ("%d", Register.rdi, generate (
	 Eint (0L, Register.rax, generate (
	 Ecall ("printf", 2, l))))))))
  | Rtl_ast.Eprintstring (r, l) ->
	 Ebinary (Xmov, r, Register.rsi, (generate (
	 Estring ("%s", Register.rdi, generate (
	 Eint (0L, Register.rax, generate (
	 Ecall ("printf", 2, l))))))))
  | Rtl_ast.Egoto l -> Egoto l
  | Rtl_ast.Eubranch (u, r, l1, l2) when ubranch_ok u ->
	 Eubranch (ubranch_to_32 u, r, l1, l2)
  | Rtl_ast.Eubranch (u, r, l1, l2) ->
	 let b, n = ubranch_to_bbranch u in
	 let r1 = Register.fresh () in
	 Eint (n, r1, generate (
	 Ebbranch (b, r, r1, l1, l2)))
  | Rtl_ast.Ebbranch (b, r1, r2, l1, l2) ->
	 Ebbranch (bbranch_to_32 b, r1, r2, l1, l2)
  | Rtl_ast.Euset (u, r1, r2, l) when ubranch_ok u ->
	 Euset (ubranch_to_32 u, r1, r2, l)
  | Rtl_ast.Euset (u, r1, r2, l) ->
	 let b, n = ubranch_to_bbranch u in
	 let r3 = Register.fresh () in
	 Eint (n, r3, generate (
	 Ebset (b, r1, r3, r2, l)))
  | Rtl_ast.Ebset (b, r1, r2, r3, l) ->
	 Ebset (bbranch_to_32 b, r1, r2, r3, l)

let func f =
  graph := LMap.empty;
  Rtl_ast.LMap.iter (fun l inst ->
   let i = instr inst in
   graph := LMap.add l i !graph) f.Rtl_ast.fun_body;
  let regs, stack = param_registers f.Rtl_ast.fun_params in
  let stack = List.mapi (fun i r -> (i, r)) stack in
  let save_registers = List.map (fun r -> r, Register.fresh ())
								Register.callee_saved in
  let entry =
	generate (Ealloc_frame (
	List.fold_right (fun (r, s) l -> move r s l) save_registers (
	List.fold_right (fun (arg, reg) l -> move reg arg l) regs (
	List.fold_right (fun (i, reg) l ->
	  generate (Eget_param (i, reg, l))) stack f.Rtl_ast.fun_entry
   )))) in
  let ex =
	let l =
	  List.fold_right (fun (r, s) l -> move s r l) save_registers (
						generate (Edelete_frame (generate Ereturn))) in
	if f.Rtl_ast.fun_has_value then
	  move f.Rtl_ast.fun_result Register.rax l
	else l
  in
  graph := LMap.add f.Rtl_ast.fun_exit (Egoto ex) !graph;
  { fun_name = f.Rtl_ast.fun_name;
	fun_params = List.length f.Rtl_ast.fun_params;
	fun_entry = entry;
	fun_body = !graph
  }

let program p =
  { prog_functions = List.map func p.Rtl_ast.prog_functions;
	prog_class_descrs = p.Rtl_ast.prog_class_descrs
  }

let next_labels = function
  | Eint (_, _, l) | Estring (_, _, l) | Eunit (_, l)
  | Egetfield (_, _, _, l) | Esetfield (_, _, _, l)
  | Ecall (_, _, l) | Ecallmethod (_, _, l)
  | Esetheader (_, _, l) | Eunary (_, _, l)
  | Ebinary (_, _, _, l) | Ebinary3 (_, _, _, _, l)
  | Ecqto l | Egoto l
  | Euset (_, _, _, l) | Ebset (_, _, _, _, l)
  | Ealloc_frame l | Edelete_frame l
  | Eget_param (_, _, l) | Epush_param (_, l)
  | Estack_free (_, l) -> [l]
  | Eubranch (_, _, l1, l2) | Ebbranch (_, _, _, l1, l2) -> [l1; l2]
  | Ereturn -> []

let print_xcbinary ff b =
  Format.fprintf ff "%s" (match b with
	| Xcadd -> "add" | Xcmul -> "imul")
				 
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
   | Ecall (f, num_args, l) ->
	  Format.fprintf ff "call %s (%d args)" f num_args
   | Ecallmethod (offset, num_args, l) ->
	  Format.fprintf ff "callm %d (%d args)" offset num_args
   | Esetheader(s, r, l) ->
	  Format.fprintf ff "set_header %a %s" Register.print r s
   | Eunary (op, r, l) ->
	  Format.fprintf ff "%a %a" print_xunary op Register.print r
   | Ebinary (op, r1, r2, l) ->
	  Format.fprintf ff "%a %a %a" print_xbinary op
					 Register.print r1 Register.print r2
   | Ebinary3 (op, r1, r2, r3, l) ->
	  Format.fprintf ff "%a %a %a %a" print_xcbinary op
			 Register.print r1 Register.print r2 Register.print r3
   | Ecqto l -> Format.fprintf ff "cqto"
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
   | Ealloc_frame l ->
	  Format.fprintf ff "alloc_frame"
   | Edelete_frame l ->
	  Format.fprintf ff "delete_frame"
   | Eget_param (n, r, l) ->
	  Format.fprintf ff "%a <- stack_param %d" Register.print r n
   | Epush_param (r, l) ->
	  Format.fprintf ff "push %a" Register.print r
   | Ereturn ->
	  Format.fprintf ff "ret"
   | Estack_free (n, l) ->
	  Format.fprintf ff "stack_free %d" n
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
  Format.fprintf ff "%s(%d)@\n" f.fun_name f.fun_params;
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
