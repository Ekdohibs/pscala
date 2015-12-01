

type 'a desc = { location : Lexing.position * Lexing.position;
				 desc : 'a
			   }
				 
type p_variance = Invariant | Covariant | Contravariant
and p_ident = string
and p_type = { type_name : p_ident;
			   arguments_type : p_type desc list
			 }
and p_param = { par_name : p_ident;
				par_type : p_type desc
			  }
and p_param_type_constraint = Any | Subtype of p_type desc | Supertype of p_type desc
and p_param_type = { param_type_name : p_ident;
					 param_type_constraint : p_param_type_constraint
				   }
and p_param_type_class = { param_type : p_param_type desc;
						   param_variance : p_variance
						 }
and p_method = { method_override : bool;
				 method_name : p_ident;
				 method_param_types : p_param_type desc list;
				 method_params : p_param desc list;
				 method_type : p_type desc;
				 method_body : p_expr desc
			   }
and unary_op = Uminus | Unot
and binary_op = Beq | Bne | Bequal | Bnotequal | Blt | Ble | Bgt | Bge | Bplus | Bminus | Btimes | Bdiv | Bmod | Band | Bor
and p_expr =
  | Eint of string
  | Estring of string
  | Ebool of bool
  | Eunit
  | Ethis
  | Enull
  | Eaccess of p_access desc
  | Eassign of p_access desc * p_expr desc
  | Ecall of p_access desc * p_type desc list * p_expr desc list
  | Enew of p_ident * p_type desc list * p_expr desc list
  | Eunary of unary_op * p_expr desc
  | Ebinary of binary_op * p_expr desc * p_expr desc
  | Eif of p_expr desc * p_expr desc * p_expr desc
  | Ewhile of p_expr desc * p_expr desc
  | Ereturn of p_expr desc option
  | Eprint of p_expr desc
  | Ebloc of p_bloc desc
and p_access = Avar of p_ident | Afield of p_expr desc * p_ident
and p_var = { var_mutable : bool;
			  var_name : p_ident;
			  var_type : p_type desc option;
			  var_expr : p_expr desc
			}
and p_decl = Dvar of p_var desc | Dmethod of p_method desc
and p_var_expr = Vvar of p_var desc | Vexpr of p_expr desc
and p_bloc = p_var_expr list
and p_class = { class_name : p_ident;
				class_type_params : p_param_type_class desc list;
				class_params : p_param desc list;
				class_decls : p_decl list;
				class_extends : (p_type desc) * (p_expr desc list)
			  }
and p_prog = { prog_classes : p_class desc list;
			   prog_main : p_decl list desc
			 }
