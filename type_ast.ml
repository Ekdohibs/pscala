open Ast

module Smap = Map.Make(String)
module Sset = Set.Make(String)
					  
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

type t_method_sig = {
  t_method_param_types : (p_ident * t_param_type_constraint) list;
  t_method_params : t_type list;
  t_method_type : t_type;
}
				  
type t_class_sig = {
  t_class_type_params : (p_ident * t_param_type_constraint * p_variance) list;
  t_class_params : (p_ident * t_type) list;
  t_class_vars : (bool * t_type) Smap.t;
  t_class_methods : t_method_sig Smap.t;
  t_class_inherits : TNset.t;
  t_class_extends : t_type;
}
					  
type t_env = {
  env_cnames : t_type_name Smap.t;
  (* Les classes de l'environnement; inclut les types sans paramètres *)
  env_classes : t_class_sig TNmap.t;
  (* Les contraintes de type *)
  env_constraints : t_type TNmap.t;
  (* Les variables, mutables ou non *)
  env_variables : (bool * t_type) Smap.t;
  env_null_inherits : TNset.t;
  env_return_type : t_type option;
}

type t_variable =
  | TLocal of (p_ident * int)
  | TParam of (p_ident * int)
  | TClassParam of (p_ident * int)

type t_expr =
	{ t_expr_type : t_type;
	  t_expr : t_expr_desc }

and t_expr_desc =
  | Tint of string
  | Tstring of string
  | Tbool of bool
  | Tunit
  | Tthis
  | Tnull
  | Taccess of t_access
  | Tassign of t_access * t_expr
  | Tcall of t_expr * p_ident * t_type list * t_expr list
  | Tnew of t_type * t_expr list
  | Tunary of unary_op * t_expr
  | Tbinary of binary_op * t_expr * t_expr
  | Tif of t_expr * t_expr * t_expr
  | Twhile of t_expr * t_expr
  | Treturn of t_expr
  | Tprint of t_expr
  | Tbloc of t_bloc

and t_access =
  | Tvar of t_variable
  | Tfield of t_expr * p_ident

and t_var = {
  t_var_mutable : bool;
  t_var_name : t_variable;
  t_var_type : t_type;
  t_var_expr : t_expr
}

and t_var_expr = TVvar of t_var | TVexpr of t_expr		  

and t_bloc = t_var_expr list

(*
type t_method = {
  t_method_param_types : 
}
 *)
