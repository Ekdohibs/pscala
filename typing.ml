open Ast
open Type_ast
exception Typing_error of
   (Format.formatter -> unit) * (Lexing.position * Lexing.position)
let type_s s = { t_type_name = TGlobal s; t_arguments_type = [] }
let sugar x = { location = Lexing.dummy_pos, Lexing.dummy_pos; desc = x }

let unique_list l =
  let l_aux = List.sort compare l in
  let rec aux = function
    | [] 	-> true
	| [_]	-> true
	| h::t when h = List.hd(t) -> false
	| h::t 	-> aux t
  in aux (List.sort compare l_aux)

module List = struct
  include List
  let iter3 f l m n =
	List.iter2 (fun a (b,c) -> f a b c) l (List.combine m n)
  let map3 f l m n =
	List.map2 (fun a (b,c) -> f a b c) l (List.combine m n)
  let iteri2 f l m =
	List.iteri (fun i (a,b) -> f i a b) (List.combine l m)
  let mapi2 f l m =
	List.mapi (fun i (a,b) -> f i a b) (List.combine l m)
end
			  
let check_unique l err_msg =
  ignore (List.fold_left (fun seen (name, loc) ->
	 if Sset.mem name seen then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
	     err_msg name), loc));
	Sset.add name seen) Sset.empty l)

let check_unique_param_types l =
  check_unique (List.map (fun p ->
	p.desc.param_type_name, p.location) l)
			   "Type parameter %s is used multiple times"

let check_unique_params l =
  check_unique (List.map (fun p ->
	p.desc.par_name, p.location) l)
			   "Parameter %s is used multiple times"

			   
let print_list f ff l =
  match l with
  | [] -> ()
  | x :: s -> f ff x; List.iter (Format.fprintf ff ",@ %a" f) s
			  
let print_type_name ff tn =
  if !Debug.enable_debug then
	match tn with
	| TGlobal s -> Format.fprintf ff "%s" s
	| TClassTvar (s, n, level) ->
	   Format.fprintf ff "!%s(#%d,%d)" s n level
	| TMethodTvar (s, n, level) ->
	   Format.fprintf ff "@@%s(#%d,%d)" s n level
  else
	match tn with
	| TGlobal s -> Format.fprintf ff "%s" s
	| TClassTvar (s, n, level) -> Format.fprintf ff "%s" s
	| TMethodTvar (s, n, level) -> Format.fprintf ff "%s" s

										 
let rec print_type ff t =
  Format.fprintf ff "%a" print_type_name t.t_type_name;
  match t.t_arguments_type with
	[] -> ()
  | _ ->
	 Format.open_hovbox 2;
	 Format.fprintf ff "[";
	 print_list print_type ff t.t_arguments_type;
	 Format.close_box ();
	 Format.fprintf ff "]"

let print_constraint ff c =
  match c with
	TAny -> ()
  | Tsubtype t -> Format.fprintf ff " <: %a" print_type t
  | Tsupertype t -> Format.fprintf ff " >: %a" print_type t

let print_method_ptype ff (t, c) =
  Format.fprintf ff "%s%a" t print_constraint c

let print_variance ff = function
  | Invariant -> ()
  | Covariant -> Format.fprintf ff "+"
  | Contravariant -> Format.fprintf ff "-"
				 
let print_class_ptype ff (t, c, v) =
  Format.fprintf ff "%a%s%a" print_variance v t print_constraint c
				 
let print_method_type ff m =
  if m.t_method_param_types <> [] then
	Format.fprintf ff "[%a]" (print_list print_method_ptype)
				   m.t_method_param_types;
  Format.fprintf ff "(%a)" (print_list print_type)
				 m.t_method_params;
  Format.fprintf ff " : %a" print_type m.t_method_type
				 
let print_method ff (name, m) =
  Format.open_hovbox 2; Format.fprintf ff "def %s" name;
  Format.fprintf ff "%a@\n" print_method_type m;
  Format.close_box ()

let print_var ff (name, (mut, t)) =
  Format.open_hovbox 2;
  Format.fprintf ff "%s %s" (if mut then "var" else "val") name;
  Format.fprintf ff " : %a@\n" print_type t;
  Format.close_box ()

let p_string ff s = Format.fprintf ff "%s" s
				   
let print_class ff (name, c) =
  Format.open_hovbox 2;
  Format.fprintf ff "class %a" print_type_name name;
  if c.t_class_type_params <> [] then
	Format.fprintf ff "[%a]" (print_list print_class_ptype)
				   c.t_class_type_params;
  Format.fprintf ff "(%a)@\n" (print_list print_type)
				 (List.map snd c.t_class_params);
  Format.fprintf ff "extends %a@\n" print_type c.t_class_extends;
  Format.fprintf ff "inherits {%a}@\n" (print_list print_type_name)
				 (TNset.elements c.t_class_inherits);
  Format.fprintf ff "{@\n";
  Format.open_hovbox 2;
  Smap.iter (fun name m -> Format.fprintf ff "%a@\n"
		 print_method (name, m)) c.t_class_methods;
  Smap.iter (fun name v -> Format.fprintf ff "%a@\n"
		 print_var (name, v)) c.t_class_vars;
  Format.close_box ();
  Format.fprintf ff "}@\n";
  Format.close_box ()

let print_env ff env =
  TNmap.iter (fun name c -> Format.fprintf ff "%a@\n"
     print_class (name, if name = TGlobal "Null" then
	   { c with t_class_inherits =
		  TNset.union c.t_class_inherits env.env_null_inherits }
						else c)) env.env_classes;
  TNmap.iter (fun name c -> Format.fprintf ff "%a >: %a@\n"
	 print_type_name name print_type c) env.env_constraints;
  Smap.iter (fun name (a, b, _) -> Format.fprintf ff "%a@\n"
	 print_var (name, (a, b))) env.env_variables


let s2f s ff = Format.fprintf ff "%s" s

let rec last l =
  match l with
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | h :: t -> last t

let list_loc l =
  match l with
  | [] -> (Lexing.dummy_pos, Lexing.dummy_pos)
  | x :: t -> (fst x.location, snd (last l).location)

let decr_level = function
  | TGlobal s -> TGlobal s
  | TClassTvar (s, n, lev) -> TClassTvar (s, n, max 0 (lev - 1))
  | TMethodTvar (s, n, lev) -> TMethodTvar (s, n, max 0 (lev - 1))
										  
let incr_level = function
  | TGlobal s -> TGlobal s
  | TClassTvar (s, n, lev) -> TClassTvar (s, n, lev + 1)
  | TMethodTvar (s, n, lev) -> TMethodTvar (s, n, lev + 1)
										  
let rec incr_level_t t =
  { t_type_name = incr_level t.t_type_name;
	t_arguments_type = List.map incr_level_t t.t_arguments_type }
										  
let rec class_subst subst t =
  match t.t_type_name with
  | TClassTvar (_, n, 0) -> List.nth subst n
  | _ -> 
	 { t_type_name = decr_level t.t_type_name;
	   t_arguments_type = List.map (class_subst subst)
								   t.t_arguments_type }

let rec method_subst subst_m subst_c t =
  match t.t_type_name with
  | TClassTvar (_, n, 0) -> List.nth subst_c n
  | TMethodTvar (_, n, 0) -> List.nth subst_m n
  | _ -> 
	 { t_type_name = decr_level t.t_type_name;
	   t_arguments_type = List.map (method_subst subst_m subst_c)
								   t.t_arguments_type }

let var_class_subst subst (b, v) =
  (b, class_subst subst v)

let constraint_class_subst subst = function
  | TAny -> TAny
  | Tsubtype t -> Tsubtype (class_subst subst t)
  | Tsupertype t -> Tsupertype (class_subst subst t)
	
let method_class_subst subst m =
  { t_method_param_types = List.map (fun (name, constr) ->
     (name, constraint_class_subst subst constr)) m.t_method_param_types;
	t_method_params = List.map (class_subst subst) m.t_method_params;
	t_method_type = class_subst subst m.t_method_type
  }
	
let rec is_subtype env t1 t2 =
  Debug.debug ">>> is_subtype <<<@\n";
  Debug.debug "%a" print_env env;
  Debug.debug "%a %a@." print_type t1 print_type t2;
  if t1.t_type_name = TGlobal "Nothing" then
	true
  else
	let c1 = TNmap.find t1.t_type_name env.env_classes in
	(t1.t_type_name = TGlobal "Null" &&
	   TNset.mem t2.t_type_name env.env_null_inherits) ||
	(* Vraiment bon ça ? *)
	(List.mem t1.t_type_name 
	   [TGlobal "Boolean"; TGlobal "Int"; TGlobal "Unit";
		TGlobal "Null"; TGlobal "String"; TGlobal "Any";
		TGlobal "AnyRef"; TGlobal "AnyVal"] && 
	  TNset.mem t2.t_type_name c1.t_class_inherits) ||
  begin
	if t1.t_type_name = t2.t_type_name then
	  let t_params = c1.t_class_type_params in
	  let tps = List.combine t1.t_arguments_type t2.t_arguments_type in
	  List.for_all2 (fun (tp1, tp2) (_, _, variance) ->
					match variance with
					| Invariant -> tp1 = tp2
					| Covariant -> is_subtype env tp1 tp2
					| Contravariant -> is_subtype env tp2 tp1
				   ) tps t_params
	else
	  let ct = c1.t_class_extends in
	  Debug.debug "%a@." print_type ct;
	  let c = TNmap.find (decr_level ct.t_type_name) env.env_classes in
	  let subst = t1.t_arguments_type in
	  Debug.debug "%a@\n@." print_class (ct.t_type_name, c);
	  let ct_subst = class_subst subst ct in
	  if TNset.mem t2.t_type_name c.t_class_inherits 
		   && is_subtype env ct_subst t2 then
		true
	  else if TNmap.mem t2.t_type_name env.env_constraints then
		not (TNset.mem t2.t_type_name c1.t_class_inherits) 
		&& (is_subtype env t1
					   (TNmap.find t2.t_type_name env.env_constraints))
	  else false
  end

let check_bounds env subst t cstr loc =
  match cstr with
	TAny -> ()
  | Tsubtype t2 ->
	 let nt = subst t2 in
	 if not (is_subtype env t nt) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff 
		 "Type argument does not comply to bounds:@ type@, %a@ should be a subtype of type@ %a" 
		 print_type t print_type nt), loc))
  | Tsupertype t2 ->
	 let nt = subst t2 in
	 if not (is_subtype env nt t) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff 
		 "Type argument does not comply to bounds:@ type@, %a@ should be a supertype of type@ %a" 
		 print_type t print_type nt), loc))
	
let rec p_to_t_type env t =
  let args = t.desc.arguments_type in
  let name = t.desc.type_name in
  let ct =
	try Smap.find name env.env_cnames
	with
	  Not_found -> raise (Typing_error 
		((fun ff -> Format.fprintf ff "Unbound class: %s" name),
		 t.location))
  in
  let c = TNmap.find ct env.env_classes in
  let p_types = c.t_class_type_params in
  if List.length args <> List.length p_types then
	raise (Typing_error 
	   ((fun ff -> Format.fprintf ff 
	   "Incorrect number of arguments for class %s: expected %d, got %d" 
	   name (List.length p_types) (List.length args)), t.location));
  let t_args = List.map (p_to_t_type env) t.desc.arguments_type in
  List.iter2
	(fun (tt, typ) (_, cstr, _) ->
	 check_bounds env (class_subst t_args) typ cstr tt.location
	) (List.combine t.desc.arguments_type t_args) p_types;
  { t_type_name = ct;
	t_arguments_type = t_args
  }

let constraint_to_t env c =
  match c with
  | Any -> TAny
  | Subtype t -> Tsubtype (p_to_t_type env t)
  | Supertype t -> Tsupertype (p_to_t_type env t)

let check_int t loc =
  if t.t_expr_type.t_type_name <> TGlobal "Int" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform arithmetic on type@, %a"
	  print_type t.t_expr_type), loc))

let check_bool t loc =
  if t.t_expr_type.t_type_name <> TGlobal "Boolean" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform boolean logic on type@, %a"
	  print_type t.t_expr_type), loc))

let recog_p_var_expr = function
  | Vvar _ -> true
  | Vexpr _ -> false

let take_vvar var = match var with
  | Vvar a -> a
  | _ -> failwith "erreur dans take_vvar"
		  
let rec expr_type env e =
  match e.desc with
  | Eint i -> { t_expr_type = type_s "Int";
				t_expr = Tint i }
  | Estring s -> { t_expr_type = type_s "String";
				   t_expr = Tstring s }
  | Ebool b -> { t_expr_type = type_s "Boolean";
				 t_expr = Tbool b }
  | Eunit -> { t_expr_type = type_s "Unit";
			   t_expr = Tunit }
  | Ethis -> let (_, typ, _) = Smap.find "this" env.env_variables in
			 { t_expr_type = typ;
			   t_expr = Tthis }
  | Enull -> { t_expr_type = type_s "Null";
			   t_expr = Tnull }
  | Eaccess acc -> let (is_mutable, a_type, access) =
					 (access_type env acc) in
				   { t_expr_type = a_type;
					 t_expr = Taccess access }
  | Eassign (acc, value) ->
	 let (is_mutable, a_type, access) = access_type env acc in
	 if not is_mutable then
	   raise (Typing_error 
	       (s2f "Trying to assign an immutable value", e.location));
	 let v = expr_type env value in
	 if not (is_subtype env v.t_expr_type a_type) then
	   raise (Typing_error 
	      ((fun ff -> Format.fprintf ff 
		  "Incorrect types in assignment:@ type@, %a@ is not a subtype of type @, %a" 
		  print_type v.t_expr_type print_type a_type), e.location));
	 { t_expr_type = type_s "Unit";
	   t_expr = Tassign (access, v) }
  | Ecall (acc, t_params, args) ->
	 let e1, name = match acc.desc with Afield (e, m) -> e, m
								  | _ -> assert false in
	 let et1 = expr_type env e1 in
	 let t1 = et1.t_expr_type in
	 let arg_types = List.map (p_to_t_type env) t_params in
	 let c = TNmap.find t1.t_type_name env.env_classes in
	 let m = try Smap.find name c.t_class_methods
			 with Not_found ->
			   raise (Typing_error ((fun ff -> Format.fprintf ff
                 "Method %s of type@, %a@ does not exist"
				 name print_type t1), e.location))
	 in
	 let subst = method_subst arg_types t1.t_arguments_type in
	 let effective_types = List.map subst m.t_method_params in 
	 if List.length effective_types <> List.length args then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Incorrect number of arguments to method %s of class %a:@ expected %d, got %d"
		 name print_type_name t1.t_type_name
		 (List.length effective_types) (List.length args)),
							e.location));
	 List.iter2 (fun (_, constr) (t, tt) ->
				 check_bounds env subst t constr tt.location
				) m.t_method_param_types
				(List.combine arg_types t_params);

	 let a_typed = List.map (fun arg ->
	   (expr_type env arg, arg.location)) args in
	 List.iter2 (fun (ea, loc) t2 ->
	   if not (is_subtype env ea.t_expr_type t2) then
		 raise (Typing_error ((fun ff -> Format.fprintf ff
		   "Argument type@, %a@ is not a subtype of expected type@, %a."
		   print_type ea.t_expr_type print_type t2), loc))
	   ) a_typed effective_types;
	 let c_basename = ref t1.t_type_name in
	 let is_global = function | TGlobal _ -> true | _ -> false in
	 while not (is_global !c_basename) do
	   c_basename := (TNmap.find !c_basename env.env_classes).
					 t_class_extends.t_type_name
	 done;
	 let cb = match !c_basename with
	   | TGlobal s -> s | _ -> assert false in
	 { t_expr_type = subst m.t_method_type;
	   t_expr = Tcall (cb, name, et1 :: (List.map fst a_typed))
	 }
  | Enew (type_name, type_args, args) ->
	 if type_name = "Main" then
	   raise (Typing_error (s2f
		 "Trying to instanciation singleton object Main", e.location));
	 let created_type = { type_name = type_name;
						  arguments_type = type_args } in
	 let ct = p_to_t_type env { desc = created_type;
								location = e.location }  in
	 let exprs = new_type env ct args e.location in
	 { t_expr_type = ct;
	   t_expr = Tnew (ct, exprs) }
  | Eunary (Uminus, e1) ->
	 let et1 = expr_type env e1 in
	 check_int et1 e.location;
	 { t_expr_type = type_s "Int";
	   t_expr = Tunary (Uminus, et1) }
  | Eunary (Unot, e1) ->
	 let et1 = expr_type env e1 in
	 check_bool et1 e.location;
	 { t_expr_type = type_s "Boolean";
	   t_expr = Tunary (Unot, et1) }
  | Ebinary ((Bplus | Bminus | Btimes | Bdiv | Bmod) as op, e1, e2) ->
	 let et1 = expr_type env e1 in
	 let et2 = expr_type env e2 in
	 check_int et1 e.location;
	 check_int et2 e.location;
	 { t_expr_type = type_s "Int";
	   t_expr = Tbinary (op, et1, et2) }
  | Ebinary ((Bequal | Bnotequal | Blt | Bgt | Ble | Bge) as op,
			   e1, e2) ->
	 let et1 = expr_type env e1 in
	 let et2 = expr_type env e2 in
	 check_int et1 e.location;
	 check_int et2 e.location;
	 { t_expr_type = type_s "Boolean";
	   t_expr = Tbinary (op, et1, et2) }
  | Ebinary ((Band | Bor) as op, e1, e2) ->
	 let et1 = expr_type env e1 in
	 let et2 = expr_type env e2 in
	 check_bool et1 e.location;
	 check_bool et2 e.location;
	 { t_expr_type = type_s "Boolean";
	   t_expr = Tbinary (op, et1, et2) }
  | Ebinary ((Beq | Bne) as op, e1, e2) ->
	 let et1 = expr_type env e1 in
	 let et2 = expr_type env e2 in
	 let c t = 
	   if not (is_subtype env t (type_s "AnyRef")) then
		 raise (Typing_error ((fun ff -> Format.fprintf ff
		   "Trying to test equality of type@, %a,@ which is not a subtype of AnyRef"
		   print_type t), e.location)) in
	 c et1.t_expr_type; c et2.t_expr_type;
	 { t_expr_type = type_s "Boolean";
	   t_expr = Tbinary (op, et1, et2) }
  | Eif (e1, e2, e3) ->
	 let et1 = expr_type env e1 in
	 let t1 = et1.t_expr_type in
	 if t1.t_type_name <> TGlobal "Boolean" then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		"Trying to use a value of type@, %a@ inside a condition."
		print_type t1), e.location));
	 let et2 = expr_type env e2 in
	 let et3 = expr_type env e3 in
	 let t2 = et2.t_expr_type in
	 let t3 = et3.t_expr_type in
	 { t_expr_type = 
		 if is_subtype env t2 t3 then
		   t3
		 else if is_subtype env t3 t2 then
		   t2
		 else
		   raise (Typing_error ((fun ff -> Format.fprintf ff
		     "Neither of types of the \"then\" branch:@, %a@ and of the \"else\" branch:@, %a@ is a subtype of the other."
		     print_type t2 print_type t2), e.location));
	   t_expr = Tif (et1, et2, et3) }
  | Ewhile (e1, e2) ->
	 let et1 = expr_type env e1 in
	 let t1 = et1.t_expr_type in
	 if t1.t_type_name <> TGlobal "Boolean" then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		"Trying to use a value of type@ %a@ inside condition of a while loop"
		print_type t1), e.location));
	 let et2 = expr_type env e2 in
	 { t_expr_type = type_s "Unit";
	   t_expr = Twhile (et1, et2) }
  | Ereturn None ->
	 (match env.env_return_type with
	   None -> raise (Typing_error
		(s2f "Trying to use return outside of a method", e.location))
	 | Some t ->
		if not (is_subtype env (type_s "Unit") t) then
		  raise (Typing_error ((fun ff -> Format.fprintf ff
		    "Trying to use return without argument in a method returning type %a:@ Unit is not a subtype of return type."
			print_type t), e.location)));
	 { t_expr_type = type_s "Nothing";
	   t_expr = Treturn { t_expr_type = type_s "Unit";
						  t_expr = Tunit }
	 }
  | Ereturn (Some e1) ->
	 let et1 = expr_type env e1 in
	 let t1 = et1.t_expr_type in
	 (match env.env_return_type with
		None -> raise (Typing_error
		  (s2f "Trying to use return outside of a method", e.location))
	  | Some t ->
		 if not (is_subtype env t1 t) then
		   raise (Typing_error ((fun ff -> Format.fprintf ff
		     "Error in return: trying to return a value of type %a,@ which is not a subtype of return type %a."
			 print_type t1 print_type t), e.location)));
	 { t_expr_type = type_s "Nothing";
	   t_expr = Treturn et1 }
  | Eprint e1 ->
	 let et1 = expr_type env e1 in
	 let t1 = et1.t_expr_type in
	 if not (List.mem t1.t_type_name
					  [TGlobal "Int"; TGlobal "String"]) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Error in print: trying to print a value of type %a,@ which is not an Int nor a String."
		 print_type t1), e.location));
	 { t_expr_type = type_s "Unit";
	   t_expr = Tprint et1 }
  | Ebloc b ->
	 let bl = b.desc in
     let l1 = List.filter recog_p_var_expr bl in
	 let l2 = List.map take_vvar l1 in
	 check_unique (List.map
	   (fun var -> (var.desc).var_name, var.location) l2)
		 "Variable %s is already defined inside this block";
     let t, bl = bloc_type env b.desc in
	 { t_expr_type = t;
	   t_expr = Tbloc bl }

and access_type env acc =
  match acc.desc with
  | Avar id ->
	 (try
		 let (mut, tt, v) = Smap.find id env.env_variables in
		 (mut, tt, Tvar v)
	  with Not_found ->
		   let _, this, _ = Smap.find "this" env.env_variables in
		   let c = TNmap.find this.t_type_name env.env_classes in
		   try
			 let (mut, tt) = Smap.find id c.t_class_vars in
			 (mut, tt, Tfield ({
								t_expr_type = this;
								t_expr = Tthis
							  }, id))
		   with Not_found ->
			 raise (Typing_error 
			  ((fun ff -> Format.fprintf ff "Unbound variable: %s" id),
				acc.location)))
  | Afield (e, id) ->
	 let et = expr_type env e in
	 let t = et.t_expr_type in
	 let c = TNmap.find t.t_type_name env.env_classes in
	 try
	   let (mut, tt) = Smap.find id c.t_class_vars in
	   (mut, class_subst t.t_arguments_type tt,
		Tfield (et, id))
	 with Not_found ->
	   if e.desc = Ethis && List.mem_assoc id c.t_class_params then
		 let (tt, index) = List.assoc id
		  (List.mapi (fun i (a, b) -> (a, (b, i))) c.t_class_params) in
		  (false, tt, Tvar (TClassParam (id, index)))
	   else
		 raise (Typing_error 
		   ((fun ff -> Format.fprintf ff 
		  "Type %a has no field %s" print_type t id), acc.location))
			 
and new_type env t args loc =
  let c = TNmap.find t.t_type_name env.env_classes in
  if List.length args <> List.length c.t_class_params then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Incorrect number of arguments of constructor of type@, %a:@ expected %d, got %d"
	  print_type t
	  (List.length c.t_class_params)
	  (List.length args)), loc));
  List.map2 (fun (_, arg_type) arg ->
	let at = class_subst t.t_arguments_type arg_type in
    let arg2 = expr_type env arg in
	let at2 = arg2.t_expr_type in
	if not (is_subtype env at2 at) then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
	    "Incorrect types in constructor of@, %a:@ type@, %a@ is not a subtype of type@, %a"
		print_type t print_type at2 print_type at), arg.location));
	arg2
  ) c.t_class_params args

and var_type env var =
  match var.desc.var_type with
  | None -> expr_type env var.desc.var_expr
  | Some t ->
	 let t = p_to_t_type env t in
	 let et2 = expr_type env var.desc.var_expr in
	 let t2 = et2.t_expr_type in
	 if not (is_subtype env t2 t) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Incorrect type declaration of variable:@ type@, %a@ is not a subtype of type@, %a"
		 print_type t2 print_type t), var.location));
	 { t_expr_type = t;
	   t_expr = et2.t_expr }

and bloc_type env b =
  match b with
  |	[] -> type_s "Unit", []
  | [Vexpr e] -> let et = expr_type env e in
				 et.t_expr_type, [TVexpr et]
  | Vexpr e :: bs ->
	 let et = expr_type env e in
	 let t, b = bloc_type env bs in
	 t, (TVexpr et) :: b
  | Vvar v :: bs ->
	 let et = var_type env v in
	 let tv = TLocal (v.desc.var_name, env.env_local_index) in
	 let nenv = { env with
	  env_variables =
		Smap.add v.desc.var_name
				 (v.desc.var_mutable, et.t_expr_type, tv)
				 env.env_variables;
	  env_local_index = env.env_local_index + 1 } in
	 let t, b = bloc_type nenv bs in
	 t, (TVvar {
			 t_var_mutable = v.desc.var_mutable;
			 t_var_name = tv;
			 t_var_type = et.t_expr_type;
			 t_var_expr = et
		   }) :: b
	 

let type_name t =
  match t with
  | TGlobal s | TMethodTvar (s, _, _) | TClassTvar (s, _, _) -> s
			   
let add_type_param_to_env env name constr =
  let extends = match constr with
	  Tsubtype t -> t
	| _ -> type_s "Any"
  in
  let class_ext = TNmap.find extends.t_type_name env.env_classes in
  let new_classes = TNmap.add name
	{
	  t_class_type_params = [];
	  t_class_params = [];
	  t_class_vars = class_ext.t_class_vars;
	  t_class_methods = class_ext.t_class_methods;
	  t_class_extends = incr_level_t extends;
	  t_class_inherits = TNset.add name class_ext.t_class_inherits
	} env.env_classes in
  { env_cnames = Smap.add (type_name name) name env.env_cnames;
	env_classes = new_classes;
	env_constraints =
	  (match constr with
		 Tsupertype t -> TNmap.add name t env.env_constraints
	   | _ -> env.env_constraints);
	env_variables = env.env_variables;
	env_null_inherits = env.env_null_inherits;
	env_return_type = None;
	env_local_index = 0;
  }

let extend_env env param_types is_method =
  let u name i = if is_method then
				   TMethodTvar (name, i, 0)
				 else
				   TClassTvar (name, i, 0)
  in
  let eenv = ref env in
  let pt = List.mapi
	(fun i param_type -> 
	 let name = param_type.desc.param_type_name in
	 let constr = constraint_to_t !eenv param_type.desc.param_type_constraint in
	 eenv := add_type_param_to_env !eenv (u name i) constr;
	 (name, constr)) param_types in
  (!eenv, pt)

(* vérification de la variance *)

let rec variance_type env name_t typ_t typ_p var_exp var =
  match (typ_t.t_type_name = name_t) with
  | true  -> if var = var_exp then () else raise 
     (Typing_error ((fun ff -> Format.fprintf ff
	 "The type %a should have been %a, but appears in a %a position"
	 print_type_name name_t rev_aux var_exp rev_aux var), typ_p.location))
  | false -> match typ_t.t_type_name with
        | TMethodTvar _ -> ()
		| _ 			->
        let cl = TNmap.find typ_t.t_type_name env.env_classes in
		List.iter3 (fun a_t a_p (_,_,b) -> 
		   variance_type env name_t a_t a_p var_exp ((aux b)*var))
	    typ_t.t_arguments_type typ_p.desc.arguments_type cl.t_class_type_params
and aux = function
  | Covariant     -> 1
  | Contravariant -> -1
  | Invariant     -> 0
and rev_aux ff = function
  | 1	-> Format.fprintf ff "covariant"
  | -1	-> Format.fprintf ff "contravariant"
  | 0	-> Format.fprintf ff "invariant"
  | _	-> assert false

let variance_constr env classe_t classe_p name_t var_exp x = function
  | (TAny,Any)					-> ()
  | (Tsubtype t_t,Subtype t_p)	-> variance_type env name_t t_t t_p var_exp x
  | (Tsupertype t_t,Supertype t_p)	-> variance_type env name_t t_t t_p 
      var_exp (-x)
  | _							-> assert false

let variance_meth env classe_t classe_p name_t var_exp var m_t m_p =
  List.iter2 (fun (_,b) a -> variance_constr env classe_t classe_p name_t 
      var_exp (-var) (b,a.desc.param_type_constraint)) 
      m_t.t_method_param_types m_p.method_param_types;
  List.iter2 (fun b a -> variance_type env name_t b a.desc.par_type 
      var_exp (-var)) m_t.t_method_params m_p.method_params ;
  variance_type env name_t m_t.t_method_type m_p.method_type var_exp var

let rec dummy_parser_type loc typ_t = 
   { location = loc ; 
     desc = { type_name = "" ; 
	          arguments_type = List.map (dummy_parser_type loc) 
			    typ_t.t_arguments_type
			 } 
	 }

let variance_sign env name_t classe_t classe_p var_exp =
  let (la,lb) = List.partition 
     (fun a -> match a with Dvar _ -> true | Dmethod _ -> false)
     classe_p.class_decls in
  let l1_p = List.map (fun a -> match a with Dvar b -> b | _ -> assert false) 
     la in
  let l2_p = List.map (fun a -> match a with Dmethod b -> b | _ -> assert false)
     lb in
  List.iter (fun a -> 
     let (b,a_t) = Smap.find a.desc.var_name classe_t.t_class_vars in
     let typ = match a.desc.var_type with
	 | None     -> dummy_parser_type a.location a_t
	 | Some typ -> typ
	 in if b then variance_type env name_t a_t typ var_exp 0 
		 else variance_type env name_t a_t typ var_exp 1)
     l1_p ;
  variance_type env name_t classe_t.t_class_extends (fst classe_p.class_extends)
     var_exp 1 ;
  List.iter (fun m -> 
     let m_t = Smap.find m.desc.method_name classe_t.t_class_methods in
	 variance_meth env classe_t classe_p name_t var_exp 1 m_t m.desc) l2_p ;
  List.iter2 (fun (_,a,_) b -> 
     variance_constr env classe_t classe_p name_t var_exp 1 
     (a,b.desc.param_type.desc.param_type_constraint))
     classe_t.t_class_type_params classe_p.class_type_params
	 
let variance env name_t classe_t classe_p i = match i with
  | 0  -> ()
  | x  -> variance_sign env name_t classe_t classe_p x 

let variance_classe env classe_t classe_p =
  List.iteri (fun i a ->
	 variance env (TClassTvar (a.desc.param_type.desc.param_type_name, i, 0))
	 classe_t classe_p (aux a.desc.param_variance)) classe_p.class_type_params
			 
let type_class env c =
  let class_env = ref env in
  let class_name = c.desc.class_name in
  if Smap.mem class_name env.env_cnames then
    raise (Typing_error ((fun ff -> Format.fprintf ff
	"Class %s is already defined" class_name), c.location)) ; 
  check_unique_param_types
	(List.map (fun p -> p.desc.param_type) c.desc.class_type_params);
  let (ce, tp) = extend_env !class_env
	(List.map (fun p -> p.desc.param_type) c.desc.class_type_params)
	false in
  let type_params = List.map2
	(fun (n, cst) t -> (n, cst, t.desc.param_variance))
	tp c.desc.class_type_params in
  class_env := ce;
  let ext = p_to_t_type !class_env (fst c.desc.class_extends) in
  if class_name <> "Main" && List.mem ext.t_type_name 
	  [TGlobal "Any"; TGlobal "AnyVal"; TGlobal "Unit";
	   TGlobal "Int"; TGlobal "Boolean"; TGlobal "String";
	   TGlobal "Null"; TGlobal "Nothing"] 
		then raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Extending builtin class %a is not allowed"
	  print_type ext), (fst c.desc.class_extends).location));
(*  let c_ext = try TNmap.find ext.t_type_name !class_env.env_classes 
  with Not_found -> 
    raise (Typing_error ((fun ff -> Format.fprintf ff
	"Class %s can't be extended since it doesn't exist" ext.t_type_name), 
	c.location)) *)
  let c_ext = TNmap.find ext.t_type_name !class_env.env_classes in
  let dummy_cls = {
	t_class_type_params = type_params;
	t_class_params = [];
	t_class_vars = Smap.map (var_class_subst ext.t_arguments_type)
							 c_ext.t_class_vars;
	t_class_methods = Smap.map (method_class_subst ext.t_arguments_type)
							   c_ext.t_class_methods;
	t_class_extends = ext;
	t_class_inherits = TNset.add (TGlobal class_name)
								 c_ext.t_class_inherits
  } in
  let cenv = { !class_env with
			   env_classes = TNmap.add (TGlobal class_name)
							   dummy_cls !class_env.env_classes;
			   env_cnames = Smap.add class_name (TGlobal class_name)
									 !class_env.env_cnames } in
  check_unique_params c.desc.class_params;
  let params = List.map (fun param -> 
        param.desc.par_name, p_to_t_type cenv param.desc.par_type) 
        c.desc.class_params in
  let cls = ref { dummy_cls with t_class_params = params } in
  let env_classes = !class_env.env_classes in
  let update_class_env () =
	class_env := { !class_env with env_classes =
	  TNmap.add (TGlobal class_name) !cls env_classes }
  in
  
  class_env := { !class_env with
				 env_null_inherits = TNset.add (TGlobal class_name)
									 !class_env.env_null_inherits;
				 env_cnames = Smap.add class_name (TGlobal class_name)
			   !class_env.env_cnames };
  update_class_env ();
  List.iteri
	(fun i (par_name, par_type) ->
	 class_env := { !class_env with
	   env_variables = Smap.add par_name
	       (false, par_type, TClassParam (par_name, i))
		   !class_env.env_variables }
	) params;

  class_env := { !class_env with
				 env_variables = Smap.add "this"
				   (false,
					{ t_type_name = (TGlobal class_name);
					  t_arguments_type =
						List.mapi (fun i (name, _, _) ->
						  { t_type_name = TClassTvar (name, i, 0);
							t_arguments_type = [] })
								 type_params
					},
				   TParam ("this", -1))
				   !class_env.env_variables };
  let ext_el =
	new_type !class_env ext (snd c.desc.class_extends)
			 (list_loc (snd c.desc.class_extends)) in
  let t_cl = ref {
				 c_type_params = tp;
				 c_params = params;
				 c_vars = [];
				 c_methods = Smap.empty;
				 c_extends = ext, ext_el;
			   } in
  let c_methods = List.map
	(function | Dmethod m -> m | _ -> assert false)
	(List.filter (function | Dmethod _ -> true | _ -> false)
				 c.desc.class_decls) in
  let methods_l = List.map
	 (fun m -> (m.desc.method_name, m.location)) c_methods
  in
  check_unique methods_l
			   "Method %s is already defined inside this class";
  let decl_var v =
	if Smap.mem v.desc.var_name !cls.t_class_vars then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
		"Trying to redefine field %s of class %s, which is already defined"
		v.desc.var_name class_name), v.location));
	let et = var_type !class_env v in
	cls := { !cls with t_class_vars =
						 Smap.add v.desc.var_name
	    						  (v.desc.var_mutable, et.t_expr_type)
								  !cls.t_class_vars };
	t_cl := { !t_cl with c_vars =
			  (v.desc.var_mutable, et.t_expr_type, v.desc.var_name)
			  :: !t_cl.c_vars }; 
	update_class_env ()
  in
  let decl_method m =
	check_unique_param_types m.desc.method_param_types;
	let (m_env, tp) = extend_env !class_env
								 m.desc.method_param_types true in
	let m_types = List.map
	  (fun p -> p_to_t_type m_env p.desc.par_type)
	  m.desc.method_params in
	let m_type = p_to_t_type m_env m.desc.method_type in
	let tm = { t_method_param_types = tp;
			   t_method_params = m_types;
			   t_method_type = m_type } in
	let prev_methods = !cls.t_class_methods in
	cls := { !cls with t_class_methods =
				   Smap.add m.desc.method_name
							tm !cls.t_class_methods };
	update_class_env ();
	let m_env = ref m_env in
	List.iteri (fun i (a, b) ->
				m_env := add_type_param_to_env !m_env
							 (TMethodTvar (a, i, 0)) b) tp;
	let m_env = ref { !m_env with env_classes =
		  TNmap.add (TGlobal class_name) !cls !m_env.env_classes } in
	
	if Smap.mem m.desc.method_name prev_methods then begin
	  if not m.desc.method_override then
		raise (Typing_error ((fun ff -> Format.fprintf ff
		  "Trying to define method %s in class %s but a method with the same name already exists in parent class %s"
		  m.desc.method_name class_name (type_name ext.t_type_name)),
							 m.location));
	  let prev_method = Smap.find m.desc.method_name
								  prev_methods in
	  if List.length tp <>
		   List.length prev_method.t_method_param_types then
		raise (Typing_error ((fun ff -> Format.fprintf ff
		   "Trying to override existing method %s of class %s with a different number of type arguments: expected %d, got %d"
		   m.desc.method_name (type_name ext.t_type_name)
		   (List.length prev_method.t_method_param_types)
		   (List.length tp)), m.location));
	  let rename_subst =  method_subst
		(List.mapi (fun i (s, _) ->
					{ t_type_name = TMethodTvar (s, i, 0);
					  t_arguments_type = [] })
				   tm.t_method_param_types)
		ext.t_arguments_type
	  in
	  let subst = class_subst ext.t_arguments_type in
	  let old_m = {
		t_method_param_types = List.map (function
		  | (s, TAny) -> (s, TAny)
		  | (s, Tsubtype t) -> (s, Tsubtype (rename_subst t))
		  | (s, Tsupertype t) -> (s, Tsupertype (rename_subst t))
		) prev_method.t_method_param_types;
		t_method_params = List.map rename_subst
								   prev_method.t_method_params;
		t_method_type = rename_subst prev_method.t_method_type
	  } in
	  let old_m_norename = {
		t_method_param_types = List.map (function
		  | (s, TAny) -> (s, TAny)
		  | (s, Tsubtype t) -> (s, Tsubtype (subst t))
		  | (s, Tsupertype t) -> (s, Tsupertype (subst t))
		) prev_method.t_method_param_types;
		t_method_params = List.map subst prev_method.t_method_params;
		t_method_type = subst prev_method.t_method_type
	  } in
	  let incomp () =
		raise (Typing_error ((fun ff -> Format.fprintf ff
		  "Trying to override existing method %s of class %s with incompatible signatures:@, %s%a@ is not compatible with existing signature@, %s%a"
		  m.desc.method_name (type_name ext.t_type_name)
		  m.desc.method_name print_method_type tm
		  m.desc.method_name print_method_type old_m_norename),
							 m.location)) in
	  if List.length old_m.t_method_params <>
		   List.length tm.t_method_params then
		incomp();
	  (* Vérification de la compatibilité des contraintes:
         On transforme les contraintes triviales en TAny *)
	  let simpl_ctr (s, ct) =
		  (s, 
		   if ct = Tsubtype (type_s "Any") then TAny
		   else if ct = Tsupertype (type_s "Nothing") then TAny
		   else ct
		  ) in
	  List.iter2
		(fun (_, ct1) (_, ct2) -> if ct1 <> ct2 then incomp ())
		(List.map simpl_ctr old_m.t_method_param_types)
		(List.map simpl_ctr tm.t_method_param_types);
	  List.iter2
		(fun t1 t2 -> if t1 <> t2 then incomp ())
		old_m.t_method_params tm.t_method_params;
	  if not (is_subtype !m_env tm.t_method_type old_m.t_method_type) then
		incomp()
    end else if m.desc.method_override then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
	    "Trying to override non-existant method %s"
		m.desc.method_name), m.location));
	check_unique_params m.desc.method_params;
	List.iteri2 (fun i p t -> m_env := { !m_env with env_variables =
			Smap.add p.desc.par_name
					 (false, t, TParam (p.desc.par_name, i))
					 !m_env.env_variables })
			   m.desc.method_params m_types;
	m_env := { !m_env with env_return_type = Some m_type };
	let et = expr_type !m_env m.desc.method_body in
	if not (is_subtype !m_env et.t_expr_type m_type) then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Incorrect method return value:@ type@, %a@ is not a subtype of type@, %a"
	  print_type et.t_expr_type print_type m_type), m.location));
	let t_meth = {
	  m_param_types = tp;
	  m_params = List.combine
		  (List.map (fun p -> p.desc.par_name) m.desc.method_params)
		  m_types;
	  m_type = m_type;
	  m_body = et;
	} in
	t_cl := { !t_cl with c_methods =
				 Smap.add m.desc.method_name t_meth !t_cl.c_methods } 
  in
  let decl = function
	| Dvar v -> decl_var v
	| Dmethod m -> decl_method m
  in
  List.iter decl c.desc.class_decls;
  variance_classe !class_env !cls c.desc ;
  { env with
	env_cnames = Smap.add class_name (TGlobal class_name)
						  env.env_cnames;
	env_classes = TNmap.add (TGlobal class_name) !cls
							env.env_classes;
	env_null_inherits = TNset.add (TGlobal class_name)
								  env.env_null_inherits},
  { !t_cl with c_vars = List.rev !t_cl.c_vars }

let make_base_class inherits extends =
  { t_class_type_params = [];
	t_class_params = [];
	t_class_vars = Smap.empty;
	t_class_methods = Smap.empty;
	t_class_inherits = List.fold_left 
	   (fun u v -> TNset.add (TGlobal v) u) TNset.empty inherits;
	t_class_extends = type_s extends }
	
let bc =
[
  "Any", make_base_class ["Any"] "Any";
  "AnyRef", make_base_class ["Any"; "AnyRef"] "Any";
  "AnyVal", make_base_class ["Any"; "AnyVal"] "Any";
  "Unit", make_base_class ["Any"; "AnyVal"; "Unit"] "AnyVal";
  "Int", make_base_class ["Any"; "AnyVal"; "Int"] "AnyVal";
  "Boolean", make_base_class ["Any"; "AnyVal"; "Boolean"] "AnyVal";
  "String", make_base_class ["Any"; "AnyRef"; "String"] "AnyRef";
  "Null", make_base_class ["Any"; "AnyRef"; "String"; "Null"] "Null";
  "Nothing", make_base_class ["Nothing"] "Nothing";
  (* Array, pour main *)
  "Array", { (make_base_class ["Array"] "Array")
		   with t_class_type_params = [(" ", TAny, Invariant)] };
]     
let base_classes =
  List.fold_left (fun m (n, v) -> TNmap.add (TGlobal n) v m)
				 TNmap.empty bc

(* On ajoute des noms fictifs avec des " " à la fin pour
   les types générés par le parser *) 
let bc_names = List.fold_left
  (fun m (n, _) -> Smap.add (n ^ " ") (TGlobal n)
				   (Smap.add n (TGlobal n) m))
  Smap.empty bc

let type_program prog =
  let base_env = {
	env_cnames = bc_names;
	env_classes = base_classes;
	env_constraints = TNmap.empty;
	env_variables = Smap.empty;
	env_null_inherits = TNset.empty;
	env_return_type = None;
	env_local_index = 0;
  } in
  let env = ref base_env in
  let classes = ref Smap.empty in
  let update cls =
	let e, c = type_class !env cls in
	env := e;
	classes := Smap.add cls.desc.class_name c !classes
  in
  List.iter update prog.prog_classes;
  let main_class = { class_name = "Main";
					 class_type_params = [];
					 class_params = [];
					 class_decls = prog.prog_main.desc;
					 class_extends = (sugar { type_name = "Any ";
											  arguments_type = [] },
									  []) } in
  update { desc = main_class;
		   location = prog.prog_main.location };
  let err s = raise (Typing_error (s2f s, prog.prog_main.location)) in
  let main_class = TNmap.find (TGlobal "Main") !env.env_classes in
  let main_method =
	try Smap.find "main" main_class.t_class_methods
	with Not_found ->
	  err "Class Main does not have a \"main\" method"
  in
  if main_method.t_method_param_types <> [] then
	err "Function main should not have any type parameter";
  if main_method.t_method_params <>
	   [{ t_type_name = TGlobal "Array";
		  t_arguments_type = [type_s "String"] }] then
	err "Function main has incorrect parameters";
  if main_method.t_method_type <> (type_s "Unit") then
	err "Function main has incorrect return type";
  !classes
