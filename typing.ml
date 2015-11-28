open Ast

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
  env_classes : t_class Smap.t;
  env_constraints : t_type Smap.t;
  env_variables : (bool * t_type) Smap.t;
}

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
	
