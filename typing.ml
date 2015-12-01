open Ast
exception Typing_error of (Format.formatter -> unit) * (Lexing.position * Lexing.position)

module Smap = Map.Make(String)
module Sset = Set.Make(String)
let typer_debug = ref false
					  
type t_type = {
  t_type_name : p_ident;
  t_arguments_type : t_type list;
}

type t_param_type_constraint =
	TAny | Tsubtype of t_type | Tsupertype of t_type

type t_method = {
  t_method_param_types : (string * t_param_type_constraint) list;
  t_method_params : t_type list;
  t_method_type : t_type;
}
				  
type t_class = {
  t_class_type_params : (string * t_param_type_constraint * p_variance) list;
  t_class_params : t_type list;
  t_class_vars : (bool * t_type) Smap.t;
  t_class_methods : t_method Smap.t;
  t_class_inherits : Sset.t;
  t_class_extends : t_type;
}
					  
type t_env = {
  env_classes : t_class Smap.t; (* Les classes de l'environnement; inclut les types sans paramètres *)
  env_constraints : t_type Smap.t; (* Les contraintes de type *)
  env_variables : (bool * t_type) Smap.t; (* Les variables, mutables ou non *)
  env_null_inherits : Sset.t;
  env_return_type : t_type option;
}

let type_s s = { t_type_name = s; t_arguments_type = [] }
let sugar x = { location = Lexing.dummy_pos, Lexing.dummy_pos; desc = x }	   

(* let debug = Format.eprintf *)
(* let debug x = Format.ifprintf Format.err_formatter x *)
let debug x = if !typer_debug then Format.eprintf x 
   else Format.ifprintf Format.err_formatter x

let print_list f ff l =
  match l with
  | [] -> ()
  | x :: s -> f ff x; List.iter (Format.fprintf ff ",@ %a" f) s
			  
let rec print_type ff t =
  Format.fprintf ff "%s" t.t_type_name;
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
  Format.open_hovbox 2; Format.fprintf ff "class %s" name;
  if c.t_class_type_params <> [] then
	Format.fprintf ff "[%a]" (print_list print_class_ptype)
				   c.t_class_type_params;
  Format.fprintf ff "(%a)@\n" (print_list print_type)
				 c.t_class_params;
  Format.fprintf ff "extends %a@\n" print_type c.t_class_extends;
  Format.fprintf ff "inherits {%a}@\n" (print_list p_string)
				 (Sset.elements c.t_class_inherits);
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
  Smap.iter (fun name c -> Format.fprintf ff "%a@\n"
     print_class (name, if name = "Null" then
						  { c with t_class_inherits = Sset.union c.t_class_inherits env.env_null_inherits }
						else c)) env.env_classes;
  Smap.iter (fun name c -> Format.fprintf ff "%s >: %a@\n"
	 name print_type c) env.env_constraints;
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
				   
let rec arg_subst subst t =
  if Smap.mem t.t_type_name subst then
	Smap.find t.t_type_name subst
  else
	{ t_type_name = t.t_type_name;
	  t_arguments_type = List.map (arg_subst subst) t.t_arguments_type }

let class_subst c args =
  List.fold_left2 (fun s (name, _, _) arg -> Smap.add name arg s) Smap.empty c.t_class_type_params args
	  
let rec is_subtype env t1 t2 =
  debug ">>> is_subtype <<<@\n";
  debug "%a" print_env env;
  debug "%a %a@\n" print_type t1 print_type t2;
  if t1.t_type_name = "Nothing" then
	true
  else
	let c1 = Smap.find t1.t_type_name env.env_classes in
	(t1.t_type_name = "Null" &&
	   Sset.mem t2.t_type_name env.env_null_inherits) ||
	(* Vraiment bon ça ? *)
	(List.mem t1.t_type_name 
	  ["Boolean"; "Int"; "Unit"; "Null"; "String"; "Any"; "AnyRef"; "AnyVal"] && 
	  Sset.mem t2.t_type_name c1.t_class_inherits) ||
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
	  let c = Smap.find ct.t_type_name env.env_classes in
	  let subst = class_subst c1 t1.t_arguments_type in
	  let ct_subst = arg_subst subst ct in
	  if Sset.mem t2.t_type_name c.t_class_inherits 
		   && is_subtype env ct_subst t2 then
		true
	  else if Smap.mem t2.t_type_name env.env_constraints then
		not (Sset.mem t2.t_type_name c1.t_class_inherits) 
		   && (is_subtype env t1 (Smap.find t2.t_type_name env.env_constraints))
	  else false
  end
	
let rec p_to_t_type env t =
  let args = t.desc.arguments_type in
  let name = t.desc.type_name in
  let c =
	try Smap.find name env.env_classes
	with
	  Not_found -> raise (Typing_error 
	     ((fun ff -> Format.fprintf ff "Unbound class: %s" name), t.location))
  in
  let p_types = c.t_class_type_params in
  if List.length args <> List.length p_types then
	raise (Typing_error 
	   ((fun ff -> Format.fprintf ff 
	   "Incorrect number of arguments for class %s: expected %d, got %d" 
	   name (List.length p_types) (List.length args)), t.location));
  let t_args = List.map (p_to_t_type env) t.desc.arguments_type in
  let subst = class_subst c t_args in
  List.iter2
	(fun (tt, typ) (_, cstr, _) ->
	 begin
	   match cstr with
		 TAny -> ()
	   | Tsubtype t2 ->
		  let nt = arg_subst subst t2 in
		  if not (is_subtype env typ nt) then
		  raise (Typing_error 
		     ((fun ff -> Format.fprintf ff 
			 "Type argument does not comply to bounds:@ type@, %a@ should be a subtype of type@ %a" 
			 print_type typ print_type nt), tt.location))
	   | Tsupertype t2 ->
		  let nt = arg_subst subst t2 in
		  if not (is_subtype env nt typ) then
		  raise (Typing_error 
		     ((fun ff -> Format.fprintf ff 
			 "Type argument does not comply to bounds:@ type@, %a@ should be a supertype of type@%a" 
			 print_type typ print_type nt), tt.location))
	 end
	) (List.combine t.desc.arguments_type t_args) p_types;
  { t_type_name = t.desc.type_name;
	t_arguments_type = t_args
  }

let constraint_to_t env c =
  match c with
  | Any -> TAny
  | Subtype t -> Tsubtype (p_to_t_type env t)
  | Supertype t -> Tsupertype (p_to_t_type env t)

let check_int t loc =
  if t.t_type_name <> "Int" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform arithmetic on type@, %a"
	  print_type t), loc))

let check_bool t loc =
  if t.t_type_name <> "Boolean" then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Trying to perform boolean logic on type@, %a"
	  print_type t), loc))

		  
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
  | Ecall (acc, t_params, args) -> assert false
  | Enew (type_name, type_args, args) ->
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
	 if t1.t_type_name <> "Boolean" then
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
	 if t1.t_type_name <> "Boolean" then
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
	 if not (List.mem t1.t_type_name ["Int"; "String"]) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff
		 "Error in print: trying to print a value of type %a,@ which is not an Int nor a String."
		 print_type t1), e.location));
	 type_s "Unit";
  | Ebloc b -> bloc_type env b.desc

and access_type env acc =
  match acc.desc with
  | Avar id ->
	 (try Smap.find id env.env_variables
	  with Not_found ->
		   let _, this = Smap.find "this" env.env_variables in
		   let c = Smap.find this.t_type_name env.env_classes in
		   try Smap.find id c.t_class_vars
		   with Not_found ->
			 raise (Typing_error 
			    ((fun ff -> Format.fprintf ff "Unbound variable: %s" id), 
				acc.location)))
  | Afield (e, id) ->
	 let t = expr_type env e in
	 let c = Smap.find t.t_type_name env.env_classes in
	 let (mut, tt) = try Smap.find id c.t_class_vars
	 with Not_found ->
	   raise (Typing_error 
	      ((fun ff -> Format.fprintf ff 
		  "Type %a has no field %s" print_type t id), acc.location))
	 in
	 let subst = class_subst c t.t_arguments_type in
	 (mut, arg_subst subst tt)
			 
and new_type env t args loc =
  let c = Smap.find t.t_type_name env.env_classes in
  let subst = class_subst c t.t_arguments_type in
  if List.length args <> List.length c.t_class_params then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Incorrect number of arguments of constructor of type@, %a:@ expected %d, got %d"
	  print_type t
	  (List.length c.t_class_params)
	  (List.length args)), loc));
  List.iter2 (fun arg_type arg ->
	let at = arg_subst subst arg_type in
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
	   
let add_type_param_to_env env name constr =
  let extends = match constr with
	  Tsubtype t -> t
	| _ -> type_s "Any"
  in (* TODO: et Null ? *)
  let class_ext = Smap.find extends.t_type_name env.env_classes in
  let new_classes = Smap.add name
	{
	  t_class_type_params = [];
	  t_class_params = [];
	  t_class_vars = class_ext.t_class_vars;
	  t_class_methods = class_ext.t_class_methods;
	  t_class_extends = extends;
	  t_class_inherits = Sset.add name class_ext.t_class_inherits
	} env.env_classes in
	  { env_classes = new_classes;
		env_constraints =
		  (match constr with
			Tsupertype t -> Smap.add name t env.env_constraints
		  | _ -> env.env_constraints);
		env_variables = env.env_variables;
		env_null_inherits = env.env_null_inherits;
		env_return_type = None;
	  }

let extend_env env param_types =
  let eenv = ref env in
  let pt = List.map
	(fun param_type -> 
	 let name = param_type.desc.param_type_name in
	 let constr = constraint_to_t !eenv param_type.desc.param_type_constraint in
	 eenv := add_type_param_to_env !eenv name constr;
	 (name, constr)) param_types in
  (!eenv, pt)


let rec variance_type env name_t typ var =
  match (typ.t_type_name = name_t) with
  | true  -> if var = 1 then () else failwith ("Bad variance of type "^name_t)
  | false -> 
     let cl = Smap.find name_t env in
	 List.iter2 (fun a (_,_,b) -> variance_type env name_t a ((aux b)*var))
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
  List.iter (fun (_,a) -> variance_constr env classe name_t (-var) a) m.t_method_param_types;
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
     classe.t_class_type_params

let variance env name_t classe i = match i with
  | 0  -> ()
  | x  -> variance_sign env name_t classe x

let variance_classe env classe =
  List.iter (fun (a,_,b) -> variance env a classe (aux b)) 
      classe.t_class_type_params

	
let type_class env c =
  let class_env = ref env in
  let class_name = c.desc.class_name in
  let (ce, tp) = extend_env !class_env
    (List.map (fun p -> p.desc.param_type) c.desc.class_type_params) in
  let type_params = List.map2
	(fun (n, cst) t -> (n, cst, t.desc.param_variance))
	tp c.desc.class_type_params in
  class_env := ce;
  let ext = p_to_t_type !class_env (fst c.desc.class_extends) in
  if List.mem ext.t_type_name ["Any"; "AnyVal"; "Unit"; "Int"; "Boolean"; "String"; "Null"; "Nothing"] then
	raise (Typing_error ((fun ff -> Format.fprintf ff
	  "Extending builtin class %s is not allowed"
	  ext.t_type_name), c.location));
  let c_ext = Smap.find ext.t_type_name !class_env.env_classes in
  let dummy_cls = {
	t_class_type_params = type_params;
	t_class_params = [];
	t_class_vars = c_ext.t_class_vars;
	t_class_methods = c_ext.t_class_methods;
	t_class_extends = ext;
	t_class_inherits = Sset.add class_name c_ext.t_class_inherits
  } in
  let cenv = { !class_env with env_classes = 
        Smap.add class_name dummy_cls !class_env.env_classes } in
  let params = List.map (fun param -> 
        param.desc.par_name, p_to_t_type cenv param.desc.par_type) 
        c.desc.class_params in
  let cls = ref { dummy_cls with t_class_params = List.map snd params } in
  let env_classes = !class_env.env_classes in
  let update_class_env () =
	class_env := { !class_env with env_classes =
	  Smap.add class_name !cls env_classes }
  in
  class_env := { !class_env with env_null_inherits =
	  Sset.add class_name !class_env.env_null_inherits };
  update_class_env ();
  List.iter
	(fun (par_name, par_type) ->
	 class_env := { !class_env with
					env_variables = Smap.add par_name (false, par_type)
											 !class_env.env_variables }
	) params;
  class_env := { !class_env with
				 env_variables = Smap.add "this"
				   (false,
					{ t_type_name = class_name;
					  t_arguments_type =
						List.map (fun (name, _, _) -> type_s name)
								 type_params
					} )
				   !class_env.env_variables };
  new_type !class_env ext (snd c.desc.class_extends)
		   (list_loc (snd c.desc.class_extends));
  let decl_var v =
	let t = var_type !class_env v in
	cls := { !cls with t_class_vars =
				   Smap.add v.desc.var_name
	    		   (v.desc.var_mutable, t) !cls.t_class_vars };
	update_class_env ()
  in
  let decl_method m =
	(* TODO: check override *)
	let (m_env, tp) = extend_env !class_env m.desc.method_param_types in
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
	let m_env = List.fold_left
	  (fun e (a, b) -> add_type_param_to_env e a b) !class_env
	  tp in
	let m_env = ref { m_env with env_classes =
			   Smap.add class_name !cls m_env.env_classes } in
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
  (* TODO: vérifier l'appel les déclarations, + la variance
     Il faut pas oublier de mettre à jour non plus la classe Null
   *)
  (try variance_classe !class_env.env_classes !cls with
  | Failure s -> raise (Typing_error ((fun ff -> Format.fprintf ff
	 "Variance error in class: %s" s), c.location)));
  { env with env_classes = Smap.add class_name !cls env.env_classes;
			 env_null_inherits = Sset.add class_name env.env_null_inherits}

let make_base_class inherits extends =
  { t_class_type_params = [];
	t_class_params = [];
	t_class_vars = Smap.empty;
	t_class_methods = Smap.empty;
	t_class_inherits = List.fold_left 
	   (fun u v -> Sset.add v u) (Sset.singleton extends) inherits;
	t_class_extends = type_s extends }
	
let base_classes =
List.fold_left (fun m (n, v) -> Smap.add n v m) Smap.empty
[
  "Any", make_base_class [] "Any";
  "AnyRef", make_base_class ["Any"] "Any";
  "AnyVal", make_base_class ["Any"] "Any";
  "Unit", make_base_class ["Any"; "AnyVal"] "AnyVal";
  "Int", make_base_class ["Any"; "AnyVal"] "AnyVal";
  "Boolean", make_base_class ["Any"; "AnyVal"] "AnyVal";
  "String", make_base_class ["Any"; "AnyRef"] "AnyRef";
  "Null", make_base_class ["Any"; "AnyRef"; "String"] "Null";
  "Nothing", make_base_class [] "Nothing";
  (* Array, pour main *)
  "Array", { (make_base_class [] "Array")
		   with t_class_type_params = [(" ", TAny, Invariant)] };
]     

let type_program prog =
  let base_env = { env_classes = base_classes;
				   env_constraints = Smap.empty;
				   env_variables = Smap.empty;
				   env_null_inherits = Sset.empty;
				   env_return_type = None;
				 } in
  let env = ref base_env in
  List.iter (fun cls -> env := type_class !env cls) prog.prog_classes;
  (* TODO: type main *)
  ()
