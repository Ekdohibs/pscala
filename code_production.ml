open Ast
open Type_ast
open X86_64

let (+++) (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
let (++@) (x1, y1) x2 = (x1 ++ x2, y1)
let enter = pushq (reg rbp) ++ movq (reg rsp) (reg rbp)
						  
let make_label = begin
	let label_index = ref 0 in
	(fun s ->
     incr label_index;
	 Format.sprintf "L_%s_%d" s !label_index)
  end

let make_data_label = begin
	let label_index = ref 0 in
	(fun s ->
     incr label_index;
	 Format.sprintf "R_%s_%d" s !label_index)
  end
				   
let method_label = begin
	let t = Hashtbl.create 17 in
	let existing = Hashtbl.create 17 in
	(fun class_name method_name ->
	 let try_create lname =
	   if Hashtbl.mem existing lname then
		 false
	   else begin
		 Hashtbl.add existing lname ();
		 Hashtbl.add t (class_name, method_name) lname;
		 true
		 end
	 in
	 try Hashtbl.find t (class_name, method_name) with
	   Not_found ->
	   let lname = ref (Format.sprintf "M_%s_%s" class_name method_name) in
	   let c = ref 1 in
	   while not (try_create !lname) do
		 lname := Format.sprintf "M_%s_%s_%d" class_name method_name !c;
		 incr c
	   done;
	   !lname)
  end

let constr_label class_name = "C_" ^ class_name

let descr_label class_name = "D_" ^ class_name

type class_repr = {
  r_num_fields : int;
  r_num_methods : int;
  r_cp_offset : int;
  r_methods : (int * string) Smap.t;
  r_lmethods : p_ident list;
  r_vars : int Smap.t;
  r_parent : p_ident
}

let make_base_repr parent = {
  r_num_fields = 0; r_num_methods = 0;
  r_methods = Smap.empty; r_vars = Smap.empty;
  r_lmethods = []; r_parent = parent;
  r_cp_offset = 0
}

let br = [
  "Any", make_base_repr "Any";
  "AnyRef", make_base_repr "Any"
]
let base_reprs =
  List.fold_left (fun m (n, v) -> Smap.add n v m) Smap.empty br
					
let add_repr c_name cls c_reprs =
  let parent_name = match (fst cls.c_extends).t_type_name with
	  TGlobal s -> s | _ -> assert false in
  let parent_repr = Smap.find parent_name c_reprs in
  let (nf, f) = List.fold_left (fun (nf, f) (_, _, name) ->
	  (nf + 1, Smap.add name nf f))
	  (parent_repr.r_num_fields + List.length cls.c_params,
	   parent_repr.r_vars) cls.c_vars in
  let (nm, lm, m) = Smap.fold (fun name _ (nm, lm, m) ->
	  try
		let (i, _) = Smap.find name m in
		(nm, lm, Smap.add name (i, c_name) m)
	  with Not_found ->
		(nm + 1, name :: lm, Smap.add name (nm, c_name) m))
	 cls.c_methods
	 (parent_repr.r_num_methods, [], parent_repr.r_methods) in
  let repr = {
	r_num_fields = nf; r_vars = f;
	r_num_methods = nm; r_methods = m;
	r_lmethods = parent_repr.r_lmethods @ (List.rev lm);
	r_cp_offset = parent_repr.r_num_fields;
	r_parent = parent_name
  } in
  Smap.add c_name repr c_reprs

let compute_reprs l_classes =
  List.fold_left (fun r (name, cls) -> add_repr name cls r)
	base_reprs l_classes

let reprs_data reprs =
  Smap.fold (fun name repr e ->
   e ++
	 label (descr_label name) ++
	 address [descr_label repr.r_parent] ++
	 address (List.map
	   (fun mn -> method_label (snd (Smap.find mn repr.r_methods)) mn)
	   repr.r_lmethods))
	reprs nop

let rec expr_locals_size expr = match expr.t_expr with
  | Tint _ | Tstring _ | Tbool _ | Tunit | Tthis | Tnull -> 0
  | Taccess a -> access_locals_size a
  | Tassign (a, e) -> max (access_locals_size a) (expr_locals_size e)
  | Tcall (_, _, l) | Tnew (_, l) ->
		List.fold_left max 0 (List.map expr_locals_size l)
  | Tunary (_, e) | Treturn e | Tprint e -> expr_locals_size e
  | Tbinary (_, e1, e2) | Twhile (e1, e2) ->
	   max (expr_locals_size e1) (expr_locals_size e2)
  | Tif (e1, e2, e3) ->
	 max (max (expr_locals_size e1) (expr_locals_size e2))
		 (expr_locals_size e3)
  | Tbloc b -> bloc_locals_size b
					   
and access_locals_size = function
  | Tvar _ -> 0
  | Tfield (e, _) -> expr_locals_size e

and bloc_locals_size = function
  | [] -> 0
  | (TVexpr e) :: b -> max (expr_locals_size e) (bloc_locals_size b)
  | (TVvar v) :: b ->
	 let s = max (expr_locals_size v.t_var_expr) (bloc_locals_size b) in
	 match v.t_var_name with
	 | TLocal (_, i) -> max s (i + 1)
	 | _ -> s

let access_arg k num_args =
  ind ~ofs:(8 * (num_args - k + 1)) rbp

let access_local k =
  ind ~ofs:(-8 * (k + 1)) rbp
	  
let set_field k =
  movq (reg rax) (ind ~ofs:(8 * (k + 1)) rsi)

let stack_reserve n =
  if n = 0 then nop else
	addq (imm (-8 * n)) (reg rsp) 

let stack_free n =
  if n = 0 then nop else
	addq (imm (8 * n)) (reg rsp) 

let create c_name reprs =
  let repr = Smap.find c_name reprs in
  movq (imm (8 * (repr.r_num_fields + 1))) (reg rdi) ++
	call "malloc" ++
	movq (ilab (descr_label c_name)) (ind rax)



let rec compile_expr expr reprs num_args = match expr.t_expr with
  | Tint i -> movq (imms i) (reg rax), nop
  | Tstring s -> let lab = make_data_label "string" in
		movq (ilab lab) (reg rax), label lab ++ string s
  | Tbool b -> movq (imm (if b then 1 else 0)) (reg rax), nop
  | Tunit -> xorq (reg rax) (reg rax), nop
  | Tnull -> xorq (reg rax) (reg rax), nop
  | Tprint e -> compile_expr e reprs num_args ++@ movq (reg rax) (reg rsi)
      ++@ movq (ilab (match e.t_expr_type.t_type_name with 
	     TGlobal "Int" -> ".Sprint_int"
		 | TGlobal "String" -> ".Sprint_string"
		 | _ -> assert false)) (reg rdi)
	  ++@ xorq (reg rax) (reg rax)
	  ++@ call "printf"
  | _ -> (nop, nop)
	   
let compile_class c_name cls reprs =
  let repr = Smap.find c_name reprs in
  let off = repr.r_cp_offset + List.length cls.c_params in
  let no_parent_constr = List.mem repr.r_parent ["Any"; "AnyRef"] in
  let locals_size =
   List.fold_left max 0 (List.map expr_locals_size
    (snd cls.c_extends @ (List.map (fun (_, e, _) -> e) cls.c_vars)))
  in
  let parent_call = if no_parent_constr then (nop, nop) else
	 let parent_cp_off = (Smap.find repr.r_parent reprs).r_cp_offset in
	 List.fold_left (+++) (nop, nop)
	  (List.mapi (fun i expr ->
		compile_expr expr reprs 0 ++@
		  movq (access_arg 0 1) (reg rsi) ++@
		  set_field (i + parent_cp_off))
		 (snd cls.c_extends)) ++@
	   pushq (access_arg 0 1) ++@
	   call (constr_label repr.r_parent) ++@
	   stack_free 1
  in
  let constr =
	(label (constr_label c_name), nop) ++@
	  enter ++@
	  stack_reserve locals_size +++
	  parent_call +++
	  List.fold_left (+++) (nop, nop)
	   (List.mapi (fun i (_, expr, _) ->
		 compile_expr expr reprs 0 ++@
		   movq (access_arg 0 1) (reg rsi) ++@
		   set_field (i + off)) cls.c_vars) ++@
	  leave ++@
	  ret
  in
  let methods = List.map (fun (name, m) ->
	let locals_size = expr_locals_size m.m_body in
	(label (method_label c_name name), nop) ++@
	  enter ++@
	  stack_reserve locals_size +++
	  compile_expr m.m_body reprs (List.length m.m_params) ++@
	  leave ++@
	  ret
	 ) (Smap.bindings cls.c_methods) in
  List.fold_left (+++) constr methods
				 
let produce_code prog =
  let reprs = compute_reprs prog in
  let r_data = reprs_data reprs in
  let (c_text, c_data) = List.fold_left (+++) (nop, nop)
	 (List.map (fun (c_name, cls) -> compile_class c_name cls reprs)
			   prog) in
  {
	text =
	  glabel "main" ++
		create "Main" reprs ++
		pushq (reg rax) ++
		call (constr_label "Main") ++
		pushq (imm 0) ++ (* Le tableau contenant les arguments, pas utilisé *)
		call (method_label "Main" "main") ++
		stack_free 2 ++

		xorq (reg rax) (reg rax) ++
		ret ++
		c_text
    ;
    data = r_data ++ c_data ++ label ".Sprint_int" ++ string "%d" 
	++ label ".Sprint_string" ++ string "%s"
  }
