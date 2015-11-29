open Ast
exception Typing_error of (Format.formatter -> unit) * (Lexing.position * Lexing.position)

module Smap = Map.Make(String)
module Sset = Set.Make(String)

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
}

let type_s s = { t_type_name = s; t_arguments_type = [] }
			   
let rec print_type ff t =
  Format.fprintf ff "%s" t.t_type_name;
  match t.t_arguments_type with
	[] -> ()
  | t1 :: ts ->
	 Format.open_hovbox 2;
	 Format.fprintf ff "[";
	 print_type ff t1;
	 List.iter (Format.fprintf ff ",@, %a" print_type) ts;
	 Format.close_box ();
	 Format.fprintf ff "]"

let s2f s ff = Format.fprintf ff "%s" s

let rec last l =
  match l with
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | h :: t -> last t
							 
let rec arg_subst subst t =
  if Smap.mem t.t_type_name subst then
	Smap.find t.t_type_name subst
  else
	{ t_type_name = t.t_type_name;
	  t_arguments_type = List.map (arg_subst subst) t.t_arguments_type }

let class_subst c args =
  List.fold_left2 (fun s (name, _, _) arg -> Smap.add name arg s) Smap.empty c.t_class_type_params args
	  
let rec is_subtype env t1 t2 =
  if t1.t_type_name = "Nothing" then
	true
  else
	let c1 = Smap.find t1.t_type_name env.env_classes in
	let c2 = Smap.find t2.t_type_name env.env_classes in 
	(t1.t_type_name = "Null" &&
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
	  if Sset.mem t2.t_type_name c.t_class_inherits && is_subtype env ct_subst t2 then
		true
	  else if Smap.mem t2.t_type_name env.env_constraints then
		not (Sset.mem t1.t_type_name c2.t_class_inherits) && (is_subtype env t1 (Smap.find t2.t_type_name env.env_constraints))
	  else false
  end
	
let rec p_to_t_type env t =
  let args = t.desc.arguments_type in
  let name = t.desc.type_name in
  let c =
	try Smap.find name env.env_classes
	with
	  Not_found -> raise (Typing_error ((fun ff -> Format.fprintf ff "Unbound class: %s" name), t.location))
  in
  let p_types = c.t_class_type_params in
  if List.length args <> List.length p_types then
	raise (Typing_error ((fun ff -> Format.fprintf ff "Incorrect number of arguments for class %s: expected %d, got %d" name (List.length p_types) (List.length args)), t.location));
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
		  raise (Typing_error ((fun ff -> Format.fprintf ff "Type argument does not comply to bounds:@ type@, %a@ should be a subtype of type@ %a" print_type typ print_type nt), tt.location))
	   | Tsupertype t2 ->
		  let nt = arg_subst subst t2 in
		  if not (is_subtype env nt typ) then
		  raise (Typing_error ((fun ff -> Format.fprintf ff "Type argument does not comply to bounds:@ type@, %a@ should be a supertype of type@%a" print_type typ print_type nt), tt.location))
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
	   raise (Typing_error (s2f "Trying to assign an immutable value", e.location));
	 let t2 = expr_type env value in
	 if not (is_subtype env t2 t) then
	   raise (Typing_error ((fun ff -> Format.fprintf ff "Incorrect types in assignment:@ type@, %a@ is not a subtype of type @, %a" print_type t2 print_type t), e.location));
	 type_s "Unit"
  | Ecall (acc, t_params, args) -> assert false
  | Enew _ -> assert false
  | Eunary _ -> assert false
  | Ebinary _ -> assert false
  | Eif _ -> assert false
  | Ewhile _ -> assert false
  | Ereturn _ -> assert false
  | Eprint _ -> assert false
  | Ebloc b -> assert false
	(*  if b.desc = [] then
	   type_s "Unit"
	 else
	   last (List.map (expr_type env) b.desc) *)

and access_type env acc =
  match acc.desc with
  | Avar id ->
	 (try Smap.find id env.env_variables
	  with Not_found ->
		   let _, this = Smap.find "this" env.env_variables in
		   let c = Smap.find this.t_type_name env.env_classes in
		   try Smap.find id c.t_class_vars
		   with Not_found ->
			 raise (Typing_error ((fun ff -> Format.fprintf ff "Unbound variable: %s" id), acc.location)))
  | Afield (e, id) ->
	 let t = expr_type env e in
	 let c = Smap.find t.t_type_name env.env_classes in
	 try Smap.find id c.t_class_vars
	 with Not_found ->
	   raise (Typing_error ((fun ff -> Format.fprintf ff "Type %a has no field %s" print_type t id), acc.location))

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
	  t_class_inherits = Sset.add extends.t_type_name class_ext.t_class_inherits
	} env.env_classes in
	  { env_classes = new_classes;
		env_constraints =
		  (match constr with
			Tsupertype t -> Smap.add name t env.env_constraints
		  | _ -> env.env_constraints);
		env_variables = env.env_variables
	  }
		
let type_class env c =
  let class_env = ref env in
  let type_params = List.map
	(fun param ->
	 let param_type = param.desc.param_type in
	 let name = param_type.desc.param_type_name in
	 let constr = constraint_to_t env param_type.desc.param_type_constraint in
	 class_env := add_type_param_to_env !class_env name constr;
	 (name, constr, param.desc.param_variance)
	) c.desc.class_type_params in
  let ext = p_to_t_type !class_env (fst c.desc.class_extends) in
  let c_ext = Smap.find ext.t_type_name !class_env.env_classes in
  let dummy_cls = {
	t_class_type_params = type_params;
	t_class_params = [];
	t_class_vars = Smap.empty;
	t_class_methods = Smap.empty;
	t_class_extends = ext;
	t_class_inherits = Sset.add ext.t_type_name c_ext.t_class_inherits
  } in
  let cenv = { !class_env with env_classes = Smap.add c.desc.class_name dummy_cls !class_env.env_classes } in
  let params = List.map (fun param -> param.desc.par_name, p_to_t_type cenv param.desc.par_type) c.desc.class_params in
  let cls = ref { dummy_cls with t_class_params = List.map snd params } in
  class_env := { !class_env with env_classes = Smap.add c.desc.class_name !cls !class_env.env_classes };
  List.iter
	(fun (par_name, par_type) ->
	 class_env := { !class_env with
					env_variables = Smap.add par_name (false, par_type)
											 !class_env.env_variables }
	) params;
  class_env := { !class_env with
				 env_variables = Smap.add "this"
				   (false,
					{ t_type_name = c.desc.class_name;
					  t_arguments_type =
						List.map (fun (name, _, _) -> type_s name)
								 type_params
					} )
				   !class_env.env_variables };
  (* TODO: vérifier l'appel à new, et les déclarations, + la variance
     Il faut pas oublier de mettre à jour non plus la classe Null
 *)
  { env with env_classes = Smap.add c.desc.class_name !cls env.env_classes }

let make_base_class inherits extends =
  { t_class_type_params = [];
	t_class_params = [];
	t_class_vars = Smap.empty;
	t_class_methods = Smap.empty;
	t_class_inherits = List.fold_left (fun u v -> Sset.add v u) Sset.empty inherits;
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
]
	
let type_program prog =
  let base_env = { env_classes = base_classes;
				   env_constraints = Smap.empty;
				   env_variables = Smap.empty } in
  let env = ref base_env in
  List.iter (fun cls -> env := type_class !env cls) prog.prog_classes;
  (* TODO: type main *)
  ()
