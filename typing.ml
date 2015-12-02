open Ast
exception Typing_error of (Format.formatter -> unit) * (Lexing.position * Lexing.position)

module Smap = Map.Make(String)
module Sset = Set.Make(String)
let typer_debug = ref false
					  
type t_type_name =
  | TGlobal of p_ident
  | TClassTvar of p_ident * int * int
  | TMethodTvar of p_ident * int * int

module TypeName = struct
  type t = t_type_name
  let compare = compare
end
module TNmap = Map.Make(TypeName)
module TNset = Set.Make(TypeName)
							   
type t_type = {
  t_type_name : t_type_name;
  t_arguments_type : t_type list;
}

type t_param_type_constraint =
	TAny | Tsubtype of t_type | Tsupertype of t_type

type t_method = {
  t_method_param_types : (p_ident * t_param_type_constraint) list;
  t_method_params : t_type list;
  t_method_type : t_type;
}
				  
type t_class = {
  t_class_type_params : (p_ident * t_param_type_constraint * p_variance) list;
  t_class_params : t_type list;
  t_class_vars : (bool * t_type) Smap.t;
  t_class_methods : t_method Smap.t;
  t_class_inherits : TNset.t;
  t_class_extends : t_type;
}
					  
type t_env = {
  env_cnames : t_type_name Smap.t;
  env_classes : t_class TNmap.t; (* Les classes de l'environnement; inclut les types sans paramètres *)
  env_constraints : t_type TNmap.t; (* Les contraintes de type *)
  env_variables : (bool * t_type) Smap.t; (* Les variables, mutables ou non *)
  env_null_inherits : TNset.t;
  env_return_type : t_type option;
}

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
				 
let print_method ff (name, m) =
  Format.open_hovbox 2; Format.fprintf ff "def %s" name;
  if m.t_method_param_types <> [] then
	Format.fprintf ff "[%a]" (print_list print_method_ptype)
				   m.t_method_param_types;
  Format.fprintf ff "(%a)" (print_list print_type)
				 m.t_method_params;
  Format.fprintf ff " : %a@\n" print_type m.t_method_type;
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
				 c.t_class_params;
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
  Smap.iter (fun name var -> Format.fprintf ff "%a@\n"
	 print_var (name, var)) env.env_variables


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
	     ((fun ff -> Format.fprintf ff "Unbound class: %s" name), t.location))
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
  if t.t_type_name <> TGlobal "Int" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform arithmetic on type@, %a"
	  print_type t), loc))

let check_bool t loc =
  if t.t_type_name <> TGlobal "Boolean" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform boolean logic on type@, %a"
	  print_type t), loc))

let recog_p_var_expr = function
  | Vvar _ -> true
  | Vexpr _ -> false

let take_vvar var = match var with
  | Vvar a -> a
  | _ -> failwith "erreur dans take_vvar"
		  
let rec expr_type env e =
  match e.desc with
  | Eint _ -> type_s "Int"
  | Estring _ -> type_s "String"
  | Ebool _ -> type_s "Boolean"
  | Eunit -> type_s "Unit"
  | Ethis -> let (_, typ) = Smap.find "this" env.env_variables in
			 typ
  | Enull -> type_s "Null"
  | Eaccess acc -> snd (access_type env acc)
  | Eassign (acc, value) ->
	 let (is_mutable, t) = access_type env acc in
	 if not is_mutable then
	   raise (Typing_error 
	       (s2f "Trying to assign an immutable value", e.location));
	 let t2 = expr_type env value in
	 if not (is_subtype env t2 t) then
	   raise (Typing_error 
	      ((fun ff -> Format.fprintf ff 
		  "Incorrect types in assignment:@ type@, %a@ is not a subtype of type @, %a" 
		  print_type t2 print_type t), e.location));
	 type_s "Unit"
  | Ecall (acc, t_params, args) ->
	 let e1, name = match acc.desc with Afield (e, m) -> e, m
								  | _ -> assert false in
	 let t1 = expr_type env e1 in
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

	 let a_types = List.map (fun arg ->
	   (expr_type env arg, arg.location)) args in
	 List.iter2 (fun (t, loc) t2 ->
	   if not (is_subtype env t t2) then
		 raise (Typing_error ((fun ff -> Format.fprintf ff
		   "Argument type@, %a@ is not a subtype of expected type@, %a."
		   print_type t print_type t2), loc))
	   ) a_types effective_types;
	 subst m.t_method_type
  | Enew (type_name, type_args, args) ->
	 if type_name = "Main" then
	   raise (Typing_error (s2f
		 "Trying to instanciation singleton object Main", e.location));
	 let created_type = { type_name = type_name;
						  arguments_type = type_args } in
	 let ct = p_to_t_type env { desc = created_type;
								location = e.location }  in
	 new_type env ct args e.location;
	 ct
  | Eunary (Uminus, e1) ->
	 check_int (expr_type env e1) e.location; type_s "Int"
  | Eunary (Unot, e1) ->
	 check_bool (expr_type env e1) e.location; type_s "Boolean"
  | Ebinary ((Bplus | Bminus | Btimes | Bdiv | Bmod), e1, e2) ->
	 check_int (expr_type env e1) e.location;
	 check_int (expr_type env e2) e.location;
	 type_s "Int"
  | Ebinary ((Bequal | Bnotequal | Blt | Bgt | Ble | Bge), e1, e2) ->
	 check_int (expr_type env e1) e.location;
	 check_int (expr_type env e2) e.location;
	 type_s "Boolean"
  | Ebinary ((Band | Bor), e1, e2) ->
	 check_bool (expr_type env e1) e.location;
	 check_bool (expr_type env e2) e.location;
	 type_s "Boolean"
  | Ebinary ((Beq | Bne), e1, e2) ->
	 let t1 = expr_type env e1 in
	 let t2 = expr_type env e2 in
	 let c t = 
	   if not (is_subtype env t (type_s "AnyRef")) then
		 raise (Typing_error ((fun ff -> Format.fprintf ff
		   "Trying to test equality of type@, %a,@ which is not a subtype of AnyRef"
		   print_type t), e.location)) in
	 c t1; c t2;
	 type_s "Boolean";
  | Eif (e1, e2, e3) ->
	 let t1 = expr_type env e1 in
	 if t1.t_type_name <> TGlobal "Boolean" then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		"Trying to use a value of type@, %a@ inside a condition."
		print_type t1), e.location));
	 let t2 = expr_type env e2 in
	 let t3 = expr_type env e3 in
	 if is_subtype env t2 t3 then
	   t3
	 else if is_subtype env t3 t2 then
	   t2
	 else
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Neither of types of the \"then\" branch:@, %a@ and of the \"else\" branch:@, %a@ is a subtype of the other."
		 print_type t2 print_type t2), e.location));
  | Ewhile (e1, e2) ->
	 let t1 = expr_type env e1 in
	 if t1.t_type_name <> TGlobal "Boolean" then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		"Trying to use a value of type@ %a@ inside condition of a while loop"
		print_type t1), e.location));
	 ignore (expr_type env e2);
	 type_s "Unit";
  | Ereturn None ->
	 (match env.env_return_type with
	   None -> raise (Typing_error
		(s2f "Trying to use return outside of a method", e.location))
	 | Some t ->
		if not (is_subtype env (type_s "Unit") t) then
		  raise (Typing_error ((fun ff -> Format.fprintf ff
		    "Trying to use return without argument in a method returning type %a:@ Unit is not a subtype of return type."
			print_type t), e.location)));
		(type_s "Nothing")
  | Ereturn (Some e1) ->
	 (match env.env_return_type with
	   None -> raise (Typing_error
		(s2f "Trying to use return outside of a method", e.location))
	  | Some t ->
		 let t1 = expr_type env e1 in
		if not (is_subtype env t1 t) then
		  raise (Typing_error ((fun ff -> Format.fprintf ff
		    "Error in return: trying to return a value of type %a,@ which is not a subtype of return type %a."
			print_type t1 print_type t), e.location)));
		(type_s "Nothing")
  | Eprint e1 -> let t1 = expr_type env e1 in
	 if not (List.mem t1.t_type_name
					  [TGlobal "Int"; TGlobal "String"]) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Error in print: trying to print a value of type %a,@ which is not an Int nor a String."
		 print_type t1), e.location));
	 type_s "Unit";
  | Ebloc b -> let bl = b.desc in
       let l1 = List.filter recog_p_var_expr bl in
	   let l2 = List.map take_vvar l1 in
	   let l3 = List.map (fun var -> (var.desc).var_name, var.location) l2 in
	   check_unique l3
		 "Variable %s is already defined inside this block";
       bloc_type env b.desc

and access_type env acc =
  match acc.desc with
  | Avar id ->
	 (try Smap.find id env.env_variables
	  with Not_found ->
		   let _, this = Smap.find "this" env.env_variables in
		   let c = TNmap.find this.t_type_name env.env_classes in
		   try Smap.find id c.t_class_vars
		   with Not_found ->
			 raise (Typing_error 
			    ((fun ff -> Format.fprintf ff "Unbound variable: %s" id), 
				acc.location)))
  | Afield (e, id) ->
	 let t = expr_type env e in
	 let c = TNmap.find t.t_type_name env.env_classes in
	 let (mut, tt) = try Smap.find id c.t_class_vars
	 with Not_found ->
	   raise (Typing_error 
	      ((fun ff -> Format.fprintf ff 
		  "Type %a has no field %s" print_type t id), acc.location))
	 in
	 (mut, class_subst t.t_arguments_type tt)
			 
and new_type env t args loc =
  let c = TNmap.find t.t_type_name env.env_classes in
  if List.length args <> List.length c.t_class_params then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Incorrect number of arguments of constructor of type@, %a:@ expected %d, got %d"
	  print_type t
	  (List.length c.t_class_params)
	  (List.length args)), loc));
  List.iter2 (fun arg_type arg ->
	let at = class_subst t.t_arguments_type arg_type in
    let at2 = expr_type env arg in
	if not (is_subtype env at2 at) then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
	    "Incorrect types in constructor of@, %a:@ type@, %a@ is not a subtype of type@, %a"
		print_type t print_type at2 print_type at), arg.location));  
  ) c.t_class_params args

and var_type env var =
  match var.desc.var_type with
  | None -> expr_type env var.desc.var_expr
  | Some t ->
	 let t = p_to_t_type env t in
	 let t2 = expr_type env var.desc.var_expr in
	 if not (is_subtype env t2 t) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Incorrect type declaration of variable:@ type@, %a@ is not a subtype of type@, %a"
		 print_type t2 print_type t), var.location));
	 t

and bloc_type env b =
  match b with
  |	[] -> type_s "Unit"
  | [Vexpr e] -> expr_type env e
  | Vexpr e :: bs -> ignore (expr_type env e); bloc_type env bs
  | Vvar v :: bs ->
	 let t = var_type env v in
	 let nenv = { env with env_variables =
	   Smap.add v.desc.var_name
				(v.desc.var_mutable, t) env.env_variables } in
	 bloc_type nenv bs

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


let rec variance_type env name_t typ var =
  match (typ.t_type_name = name_t) with
  | true  -> if var = 1 then () else failwith ("Bad variance of type "^(type_name name_t))
  | false ->
	 match typ.t_type_name with
	 (* On a un argument de méthode: pas de problème *)
	 |  TMethodTvar _ -> ()
	 | _ -> 
		let cl = TNmap.find typ.t_type_name env.env_classes in
		Debug.debug "%a %a %a@." print_type_name name_t
					print_class (TGlobal " ", cl) print_type typ;
		List.iter2 (fun a (_,_,b) -> variance_type env name_t
												   a ((aux b)*var))
	    typ.t_arguments_type cl.t_class_type_params
and aux = function
  | Covariant     -> 1
  | Contravariant -> -1
  | Invariant     -> 0

let variance_constr env classe name_t x = function
  | TAny			-> ()
  | Tsubtype t		-> variance_type env name_t t x
  | Tsupertype t	-> variance_type env name_t t (-x)

let variance_meth env classe name_t var m =
  List.iter (fun (_,a) -> 
     variance_constr env classe name_t (-var) a) m.t_method_param_types;
  List.iter (fun a -> variance_type env name_t a (-var)) m.t_method_params ;
  variance_type env name_t m.t_method_type var

let variance_sign env name_t classe x =
  Smap.iter (fun a (b,c) -> 
     if b then variance_type env name_t c 0 else variance_type env name_t c x)
     classe.t_class_vars ;
  variance_type env name_t classe.t_class_extends x ;
  Smap.iter (fun a b -> variance_meth env classe name_t x b) 
     classe.t_class_methods ;
  List.iter (fun (a,b,c) -> variance_constr env classe name_t x b)
     classe.t_class_type_params;
  List.iter (fun p -> variance_type env name_t p x)
     classe.t_class_params

	 
let variance env name_t classe i = match i with
  | 0  -> ()
  | x  -> variance_sign env name_t classe x

let variance_classe env classe =
  List.iteri (fun i (a,_,b) ->
	 variance env (TClassTvar (a, i, 0)) classe (aux b)) 
			 classe.t_class_type_params
			 
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
  let cls = ref { dummy_cls with t_class_params = List.map snd params } in
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
  List.iter
	(fun (par_name, par_type) ->
	 class_env := { !class_env with
					env_variables = Smap.add par_name (false, par_type)
											 !class_env.env_variables }
	) params;
  (*
  List.iter
	(fun (par_name, par_type) ->
	 cls := { !cls with
			  t_class_vars = Smap.add par_name (false, par_type)
									  !cls.t_class_vars }
	) params;
  update_class_env ();
  *)
  class_env := { !class_env with
				 env_variables = Smap.add "this"
				   (false,
					{ t_type_name = (TGlobal class_name);
					  t_arguments_type =
						List.mapi (fun i (name, _, _) ->
						  { t_type_name = TClassTvar (name, i, 0);
							t_arguments_type = [] })
								 type_params
					} )
				   !class_env.env_variables };
  new_type !class_env ext (snd c.desc.class_extends)
		   (list_loc (snd c.desc.class_extends));
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
	let t = var_type !class_env v in
	cls := { !cls with t_class_vars =
				   Smap.add v.desc.var_name
	    		   (v.desc.var_mutable, t) !cls.t_class_vars };
	update_class_env ()
  in
  let decl_method m =
	(* TODO: check override *)
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
	check_unique_params m.desc.method_params;
	List.iter2 (fun p t -> m_env := { !m_env with env_variables =
		   Smap.add p.desc.par_name (false, t) !m_env.env_variables })
			   m.desc.method_params m_types;
	m_env := { !m_env with env_return_type = Some m_type };
	let t = expr_type !m_env m.desc.method_body in
	if not (is_subtype !m_env t m_type) then
	  raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Incorrect method return value:@ type@, %a@ is not a subtype of type@, %a"
	  print_type t print_type m_type), m.location))
  in
  let decl = function
	| Dvar v -> decl_var v
	| Dmethod m -> decl_method m
  in
  List.iter decl c.desc.class_decls;
  (* TODO: better variance error report
   *)
  (try variance_classe !class_env !cls with
  | Failure s -> raise (Typing_error ((fun ff -> Format.fprintf ff
	 "Variance error in class: %s" s), c.location)));
  { env with
	env_cnames = Smap.add class_name (TGlobal class_name)
						  env.env_cnames;
	env_classes = TNmap.add (TGlobal class_name) !cls
							env.env_classes;
	env_null_inherits = TNset.add (TGlobal class_name)
								  env.env_null_inherits}

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
  } in
  let env = ref base_env in
  List.iter (fun cls -> env := type_class !env cls) prog.prog_classes;
  let main_class = { class_name = "Main";
					 class_type_params = [];
					 class_params = [];
					 class_decls = prog.prog_main.desc;
					 class_extends = (sugar { type_name =  "Any";
											  arguments_type = [] },
									  []) } in
  env := type_class !env { desc = main_class;
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
	err "Function main has incorrect return type"
