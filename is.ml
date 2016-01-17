open Is_ast
open Common
	   
let create_method_name = begin
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

let create_constr_name class_name = "C_" ^ class_name

module Smap = Map.Make(String)

type method_repr = {
  m_id : int;
  m_class : string;
  m_tparams : expr_type list;
  m_ntparams : int;
  mutable m_has_override : bool;
}
					  
type class_repr = {
  r_num_fields : int;
  r_num_methods : int;
  r_cp_offset : int;
  r_methods : method_repr Smap.t;
  r_lmethods : string list;
  r_vars : int Smap.t;
  r_parent : string;
  r_tparams : expr_type list;
  r_tpindex : int;
  r_gtparams : expr_type list -> expr_type list
}

let make_base_repr parent = {
  r_num_fields = 0; r_num_methods = 0;
  r_methods = Smap.empty; r_vars = Smap.empty;
  r_lmethods = []; r_parent = parent;
  r_cp_offset = 0; r_tparams = []; r_tpindex = 0;
  r_gtparams = fun l -> assert (l = []); []
}

let br = [
  "Any", make_base_repr "Any";
  "AnyRef", make_base_repr "Any"
]
let base_reprs =
  List.fold_left (fun m (n, v) -> Smap.add n v m) Smap.empty br
					
let add_repr c_name cls c_reprs =
  let open Type_ast in
  let parent_name = match (fst cls.c_extends).t_type_name with
	  TGlobal s -> s | _ -> assert false in
  let parent_repr = Smap.find parent_name c_reprs in
  let (nf, f) = List.fold_left (fun (nf, f) (_, _, name) ->
	  (nf + 1, Smap.add name nf f))
	  (parent_repr.r_num_fields + List.length cls.c_params,
	   parent_repr.r_vars) cls.c_vars in
  let (nm, lm, m) = Smap.fold (fun name meth (nm, lm, m) ->
	  try
		let mm = Smap.find name m in
		mm.m_has_override <- true;
		(nm, lm, Smap.add name { m_id = mm.m_id;
								 m_class = c_name;
								 m_tparams = mm.m_tparams;
								 m_ntparams = mm.m_ntparams;
								 m_has_override = false } m)
	  with Not_found ->
		(* TODO: be more intelligent here *)
		let tparams = List.mapi (fun i _ -> TMethodParam i)
								meth.m_param_types in
		let ntparams = List.fold_left (fun r t ->
			Is_ast.(match t with
			 | Tint | Tpointer | TClassParam _ -> r
			 | TMethodParam n -> max r (n + 1)))
					0 tparams in
		(nm + 1, name :: lm, Smap.add name
		   { m_id = nm;
			 m_class = c_name;
			 m_tparams = tparams; 
			 m_ntparams = ntparams;
			 m_has_override = false } m))
	 cls.c_methods
	 (parent_repr.r_num_methods, [], parent_repr.r_methods) in
  let repr = {
	r_num_fields = nf; r_vars = f;
	r_num_methods = nm; r_methods = m;
	r_lmethods = parent_repr.r_lmethods @ (List.rev lm);
	r_cp_offset = parent_repr.r_num_fields;
	r_parent = parent_name;
	(* TODO: be more intelligent here *)
	r_tparams = List.mapi (fun i _ ->
						   Is_ast.TClassParam (i + parent_repr.r_tpindex))
						  cls.c_type_params;
	r_tpindex = parent_repr.r_tpindex + List.length cls.c_type_params;
	r_gtparams = fun tp ->
	   let p_tparams = List.map (fun t ->
		  match t.t_type_name with
		  | TGlobal s ->
			if List.mem s ["Any"; "AnyVal"; "Int"; "Unit";
						   "Boolean"; "Nothing"] then
			  Is_ast.Tint
			else
			  Is_ast.Tpointer
		  | TClassTvar (_, i, _) ->
			 List.nth tp i
		  | _ -> assert false) ((fst cls.c_extends).t_arguments_type) in
	   (parent_repr.r_gtparams p_tparams) @ tp
  } in
  Smap.add c_name repr c_reprs

let compute_reprs l_classes =
  List.fold_left (fun r (name, cls) -> add_repr name cls r)
	base_reprs l_classes

let class_descrs reprs =
  List.map (fun (name, repr) ->
	 name, repr.r_parent,
	 List.map (fun mname ->
			   create_method_name (
				 (Smap.find mname repr.r_methods).m_class
				 ) mname) repr.r_lmethods)
		   (Smap.bindings reprs)

module Imap = Map.Make(struct type t = int let compare = compare end)
type expr_env = {
  env_reprs : class_repr Smap.t;
  env_locals_index : int;
  env_locals : int Imap.t;
  env_current_class : string;
  env_current_method : string
}

				  
let make_type t e = {
  is_expr = e;
  is_type = t
}
let make_int = make_type Tint
let make_pointer = make_type Tpointer
let unit = make_int Eunit		 

let rec make_bloc b =
  let rec flatten_bloc_list = function
	| [] -> []
	| e :: t ->
	   let tt = flatten_bloc_list t in
	   match e.is_expr with
	   | Eunit -> tt
	   | Ebloc b -> (flatten_bloc_list b) @ tt
	   | _ -> e :: tt
  in
  let rec make_bloc_ignore = function
	| [] -> [], Tint
	| [e] -> [e], e.is_type
	| e :: t ->
	   let tt, tp = make_bloc_ignore t in
	   flatten_bloc_list [ignore_result e;
						  make_type tp (Ebloc tt)], tp
  in
  match make_bloc_ignore (flatten_bloc_list b) with
  | [], _ -> unit
  | bb, tp -> make_type tp (Ebloc bb)

and ignore_result e =
  match e.is_expr with
  | Eint _ -> unit
  | Estring _ -> unit
  | Eunit -> unit
  | Egetlocal _ -> unit
  | Esetlocal _ -> e
  | Egetfield (e, _) -> ignore_result e
  | Esetfield _ -> e
  | Ecall _ -> make_bloc [e; unit]
  | Ecallmethod _ -> make_bloc [e; unit]
  | Eallocbloc _ -> unit
  | Eunary (_, e) -> ignore_result e
  | Ebinary (_, e1, e2) -> make_bloc [ignore_result e1;
									  ignore_result e2;
									  unit]
  | Eand (e1, e2) -> make_int (Eif (e1, ignore_result e2, unit))
  | Eor (e1, e2) -> make_int (Eif (e1, unit, ignore_result e2))
  | Eif (e1, e2, e3) ->
	 make_int (Eif (e1, ignore_result e2, ignore_result e3))
  | Ewhile _ -> e
  | Ereturn _ -> e
  | Eprintint _ -> e
  | Eprintstring _ -> e
  | Ebloc b -> make_bloc (List.map ignore_result b)

let make_blocn b = (make_bloc b).is_expr
						 
let make_uminus e = match e.is_expr with
  | Eint n -> Eint (Int64.neg n)
  | Eunary (Xneg, e) -> e.is_expr
  | _ -> Eunary (Xneg, e)

let make_unot e = match e.is_expr with
  | Eint n -> Eint (Int64.logxor n 1L)
  | Eunary (Xnot, e) -> e.is_expr
  | _ -> Eunary (Xnot, e)

let rec make_bplus e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, Eint m -> Eint (Int64.add n m)
  | Eint n, Eunary (Xaddi m, e) | Eunary (Xaddi m, e), Eint n ->
	  make_bplus e (make_int (Eint (Int64.add n m)))
  | Eint 0L, e | e, Eint 0L -> e
  | Eint n, e | e, Eint n -> Eunary (Xaddi n, make_int e)
  | Eunary (Xaddi n, { is_expr = e1; _ }), e2
  | e1, Eunary (Xaddi n, { is_expr = e2; _ }) ->
	 make_bplus (make_int (Eint n))
	  (make_int (make_bplus (make_int e1) (make_int e2)))
  | _, Eunary (Xneg, e2) -> make_bminus e1 e2
  | Eunary (Xneg, e1), _ -> make_brminus e1 e2
  | _ -> Ebinary (Xadd, e1, e2)

and make_bminus e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | _, Eint n -> make_bplus e1 (make_int (Eint (Int64.neg n)))
  | Eint 0L, _ -> Eunary (Xneg, e2)
  | _, Eunary (Xneg, e2) -> make_bplus e1 e2
  | Eunary (Xneg, e1), _ -> make_uminus (make_int (make_bplus e1 e2))
  | _ -> Ebinary (Xsub, e1, e2)

and make_brminus e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, _ -> make_bplus e2 (make_int (Eint (Int64.neg n)))
  | _, Eint 0L -> Eunary (Xneg, e1)
  | Eunary (Xneg, e1), _ -> make_bplus e1 e2
  | _, Eunary (Xneg, e2) -> make_uminus (make_int (make_bplus e1 e2))
  | _ -> Ebinary (Xrsub, e1, e2)
				 
let make_btimes e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, Eint m -> Eint (Int64.mul n m)
  | Eint 0L, e | e, Eint 0L -> make_blocn [ignore_result (make_int e);
										   make_int (Eint 0L)]
  | Eint 1L, e | e, Eint 1L -> e
  | Eint (-1L), e | e, Eint (-1L) -> make_uminus (make_int e)
  | Eint n, e | e, Eint n -> Eunary (Xmuli n, make_int e)
  | _ -> Ebinary (Xmul, e1, e2)

let make_bdiv e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | _ -> Ebinary (Xdiv, e1, e2)

let make_bmod e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | _ -> Ebinary (Xmod, e1, e2)
		  
let make_beq e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, e | e, Eint n -> Eunary (Xeqi n, make_int e)
  | _ -> Ebinary (Xeq, e1, e2)

let make_bne e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, e | e, Eint n -> Eunary (Xnei n, make_int e)
  | _ -> Ebinary (Xne, e1, e2)

let make_blt e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, _ -> Eunary (Xgti n, e2)
  | _, Eint n -> Eunary (Xlti n, e1)
  | _ -> Ebinary (Xlt, e1, e2)

let make_ble e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, _ -> Eunary (Xgei n, e2)
  | _, Eint n -> Eunary (Xlei n, e1)
  | _ -> Ebinary (Xle, e1, e2)

let make_bgt e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, _ -> Eunary (Xlti n, e2)
  | _, Eint n -> Eunary (Xgti n, e1)
  | _ -> Ebinary (Xgt, e1, e2)
				 
let make_bge e1 e2 =
  match (e1.is_expr, e2.is_expr) with
  | Eint n, _ -> Eunary (Xlei n, e2)
  | _, Eint n -> Eunary (Xgei n, e1)
  | _ -> Ebinary (Xge, e1, e2)

let make_band e1 e2 =
  Eand (e1, e2)

let make_bor e1 e2 =
  Eor (e1, e2)

let get_type env t =
  match t.Type_ast.t_type_name with
  | Type_ast.TGlobal s ->
	 if List.mem s ["Any"; "AnyVal"; "Int"; "Unit"; "Boolean"; "Nothing"] then
	   Tint
	 else
	   Tpointer
  | Type_ast.TClassTvar (_, index, _) ->
	 let repr = Smap.find env.env_current_class env.env_reprs in
	 List.nth repr.r_tparams index
  | Type_ast.TMethodTvar (_, index, _) ->
	 let repr = Smap.find env.env_current_class env.env_reprs in
	 let m = Smap.find env.env_current_method repr.r_methods in
	 List.nth m.m_tparams index
	  
let rec expr env e =
  let nenv, ee =
    match e.Type_ast.t_expr with
	| Type_ast.Tint i -> env, Eint (Int64.of_string i)
	| Type_ast.Tstring s -> env, Estring s
	| Type_ast.Tbool b -> env, Eint (if b then 1L else 0L)
	| Type_ast.Tunit -> env, Eunit
	| Type_ast.Tnull -> env, Eint 0L
	| Type_ast.Tprint e ->
	   let nenv, ne = expr env e in
		Type_ast.(match e.t_expr_type.t_type_name with
				  | Type_ast.TGlobal "Int" -> nenv, Eprintint ne
				  | Type_ast.TGlobal "String" -> nenv, Eprintstring ne
				  | _ -> assert false)
	| Type_ast.Tthis -> env, Egetlocal 0
	| Type_ast.Treturn e -> let nenv, ne = expr env e in
							nenv, Ereturn ne
	| Type_ast.Taccess (Type_ast.Tvar (Type_ast.TLocal (_, i))) ->
	   env, Egetlocal (Imap.find i env.env_locals)
	| Type_ast.Taccess (Type_ast.Tvar (Type_ast.TParam (_, i))) ->
	   env, Egetlocal (i + 1)
	| Type_ast.Taccess (Type_ast.Tvar (Type_ast.TClassParam (_, i, c_name))) ->
	   let repr = Smap.find c_name env.env_reprs in
	   env, Egetfield(make_pointer (Egetlocal 0), i + repr.r_cp_offset)
	| Type_ast.Taccess (Type_ast.Tfield (e, field_name, class_name)) ->
	   let repr = Smap.find class_name env.env_reprs in
	   let field = Smap.find field_name repr.r_vars in
	   let nenv, ne = expr env e in
	   nenv, Egetfield(ne, field)
	| Type_ast.Tassign (Type_ast.Tvar (Type_ast.TLocal (_, i)), e) ->
	   let nenv, ne = expr env e in
	   nenv, Esetlocal (Imap.find i env.env_locals, ne)
	| Type_ast.Tassign (Type_ast.Tvar _, _) -> assert false
	| Type_ast.Tassign (Type_ast.Tfield (e1, field_name, class_name), e2) ->
	   let repr = Smap.find class_name env.env_reprs in
	   let field = Smap.find field_name repr.r_vars in
	   let nenv1, ne1 = expr env e1 in
	   let nenv2, ne2 = expr nenv1 e2 in
	   nenv2, Esetfield(ne1, field, ne2)
	| Type_ast.Tnew (t, params) ->
	   let class_name = match t.Type_ast.t_type_name with
		   Type_ast.TGlobal name -> name
		 | _ -> assert false in
	   let repr = Smap.find class_name env.env_reprs in
	   let size = repr.r_num_fields + 1 in
	   let nenv, np = List.fold_map expr env params in
	   let tp = repr.r_gtparams
		 (List.map (get_type env) t.Type_ast.t_arguments_type) in
	   let i = nenv.env_locals_index in
	   { nenv with env_locals_index = i + 1},
	   Ebloc [
		   make_int (Esetlocal (i,
			   make_pointer (Eallocbloc (class_name, tp, size))));
		   make_int (Ecall (create_constr_name class_name, [],
							(make_pointer (Egetlocal i)) :: np));
		   make_pointer (Egetlocal i)
		 ]
	| Type_ast.Tcall (class_name, method_name, args, t_params) ->
	   let repr = Smap.find class_name env.env_reprs in
	   let meth = Smap.find method_name repr.r_methods in
	   let tp = Array.make meth.m_ntparams Tint in
	   List.iter2 (fun t tt ->
		 match tt with
		 | TMethodParam n -> tp.(n) <- get_type env t
		 | _ -> ()) t_params meth.m_tparams;
	   let nenv, np = List.fold_map expr env args in
	   nenv,
	   if meth.m_has_override || not !Options.undefined_null_deref then
		 Ecallmethod (meth.m_id, Array.to_list tp, np)
	   else
		 Ecall (create_method_name meth.m_class method_name,
				Array.to_list tp, np)
	| Type_ast.Tbloc b ->
	   if b = [] then env, Eunit else
		 let nenv, nb =
		   List.fold_map (fun env ve -> match ve with
		   | Type_ast.TVvar v ->
			  let e = v.Type_ast.t_var_expr in
			  let i = Type_ast.(match v.t_var_name with
			   TLocal (_, i) -> i | _ -> assert false) in
			  let j = env.env_locals_index in
			  let nenv = { env with env_locals_index = j + 1 } in
			  let nenv2, ne = expr nenv e in
			  { nenv2 with env_locals = Imap.add i j nenv2.env_locals },
			    make_int (Esetlocal (j, ne))
		   | Type_ast.TVexpr e ->
			  expr env e
						 ) env b in
		 { nenv with env_locals = env.env_locals }, make_blocn nb
	| Type_ast.Tif (e1, e2, e3) ->
	   let nenv1, ne1 = expr env e1 in
	   let nenv2, ne2 = expr nenv1 e2 in
	   let nenv3, ne3 = expr nenv2 e3 in
	   nenv3, Eif (ne1, ne2, ne3)
	| Type_ast.Twhile (e1, e2) ->
	   let nenv1, ne1 = expr env e1 in
	   let nenv2, ne2 = expr nenv1 e2 in
	   nenv2, Ewhile (ne1, ne2)
	| Type_ast.Tunary (op, e) ->
	   let nenv, ne = expr env e in
	   nenv, Ast.(match op with
				  | Uminus -> make_uminus
				  | Unot -> make_unot) ne
	| Type_ast.Tbinary (op, e1, e2) ->
	   let nenv1, ne1 = expr env e1 in
	   let nenv2, ne2 = expr nenv1 e2 in
	   nenv2, Ast.(match op with
		| Bplus -> make_bplus | Bminus -> make_bminus
		| Btimes -> make_btimes | Bdiv -> make_bdiv | Bmod -> make_bmod
		| Bequal | Beq -> make_beq | Bnotequal | Bne -> make_bne
		| Blt -> make_blt | Ble -> make_ble | Bgt -> make_bgt | Bge -> make_bge
		| Band -> make_band | Bor -> make_bor) ne1 ne2
  in
  nenv, make_type (get_type env e.Type_ast.t_expr_type) ee
	
let make_empty_env reprs num_args class_name method_name =
  { env_reprs = reprs;
	env_locals_index = num_args;
	env_locals = Imap.empty;
	env_current_class = class_name;
	env_current_method = method_name
  }
											 
let class_functions c_name cls reprs =
  let repr = Smap.find c_name reprs in
  let cp_off = repr.r_cp_offset in
  let num_args = List.length cls.Type_ast.c_params in
  let off = cp_off + List.length cls.Type_ast.c_params in
  let no_parent_constr = List.mem repr.r_parent ["Any"; "AnyRef"] in
  let constr_env = make_empty_env reprs (num_args + 1) c_name "" in
  let parent_call, constr_env =
	if no_parent_constr then [], constr_env else
	  let constr_env, args =
		List.fold_map expr constr_env (snd cls.Type_ast.c_extends) in
	  [make_int (Ecall (create_constr_name repr.r_parent, [],
						(make_pointer (Egetlocal 0)) :: args))],
	  constr_env
  in
  let constr_env, set_fields =
	List.fold_mapi (fun i env (_, e, _) ->
					let nenv, ne = expr env e in
					nenv, make_int (Esetfield (
							 make_pointer (Egetlocal 0), i + off, ne)))
				   constr_env cls.Type_ast.c_vars in
  let constructor = {
	fun_name = create_constr_name c_name;
	fun_params = num_args + 1;
	fun_tparams = 0;
	fun_body = make_bloc (
	   List.mapi (fun i (_, t) ->
		 make_int (Esetfield (make_pointer (Egetlocal 0),
							  i + repr.r_cp_offset,
							  make_type (get_type constr_env t)
										(Egetlocal (i + 1))))
	   ) cls.Type_ast.c_params @
	   parent_call @
	   set_fields   
	);
	fun_has_value = false;
  } in
  let methods = List.map (fun (name, m) ->
     let num_args = List.length m.Type_ast.m_params + 1 in
     { fun_name = create_method_name c_name name;
	   fun_params = num_args;
	   fun_tparams = (Smap.find name repr.r_methods).m_ntparams;
	   fun_body = snd (expr (make_empty_env reprs num_args c_name name)
							m.Type_ast.m_body);
	   fun_has_value = Type_ast.(m.m_type.t_type_name <> TGlobal "Unit")
	 }) (Smap.bindings cls.Type_ast.c_methods) in
  constructor :: methods
				 
let program prog =
  let reprs = compute_reprs prog in
  let c_descrs = class_descrs reprs in
  let prog_functions = List.flatten (List.map (fun (c_name, cls) ->
	   class_functions c_name cls reprs) prog) in
  let main_func = {
	fun_name = "main";
	fun_params = 0;
	fun_tparams = 0;
	fun_body =
	  (let repr_main = Smap.find "Main" reprs in
	   make_bloc [
		 make_int (Esetlocal (0,
		 make_pointer (Eallocbloc ("Main", [], repr_main.r_num_fields + 1))));
		 make_int (Ecall (create_constr_name "Main", [],
						  [make_pointer (Egetlocal 0)]));
		 make_int (Ecall (create_method_name "Main" "main", [],
			 [make_pointer (Egetlocal 0);
			  make_pointer (Eint 0L)]));
		 make_int (Eint 0L)
	   ]);
	fun_has_value = true;
  } in
  { prog_functions = main_func :: prog_functions;
	prog_class_descrs = c_descrs
  }
	
