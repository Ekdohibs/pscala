

type 'a desc = { location : Lexing.position;
				 desc : 'a
			   }
and p_variance = Invariant | Covariant | Contravariant
and p_ident = string
and p_type = { type_name : p_ident;
			   arguments_type : p_type list
			 }
and p_param = { par_name : p_ident;
				par_type : p_type
			  }
and p_param_type_constraint = Any | Subtype of p_type | Supertype of p_type
and p_param_type = { param_type_name : p_ident;
					 param_type_constaint : p_param_type_constraint
				   }
and p_param_type_class = { param_type : p_param_type;
						   param_variance : p_variance
						 }
and p_method = { method_override : bool;
				 method_name : p_ident;
				 method_param_types : p_param_type list;
				 method_params : p_param list;
				 method_type : p_type;
				 method_body : p_expr desc
			   }
and unary_op = Uminus | Unot
and binary_op = Beq | Bne | Bequal | Bnotequal | Blt | Ble | Bgt | Bge | Bplus | Bminus | Btimes | Bdiv | Bmod | Band | Bor
and p_expr = Eint of int
		   | Estring of string
		   | Ebool of bool
		   | Eunit
		   | Ethis
		   | Enull
		   | Eaccess
		   | Eassign of p_access desc * p_expr desc
		   | Ecall of p_access desc * p_type list * p_expr desc list
		   | Enew of p_ident * p_type list * p_expr desc list
		   | Eunary of unary_op * p_expr desc
		   | Ebinary of binary_op * p_expr desc * p_expr desc
		   | Eif of p_expr desc * p_expr desc * p_expr desc
		   | Ewhile of p_expr desc * p_expr desc
		   | Ereturn of p_expr desc option
		   | Eprint of p_expr desc
		   | Ebloc of p_bloc
and p_access = Avar of p_ident | Afield of p_expr desc * p_ident
and p_var = { var_mutable : bool;
			  var_name : p_ident;
			  var_type : p_type option;
			  var_expr : p_expr desc
			}
and p_decl = Dvar of p_var | Dmethod of p_method
and p_var_expr = Vvar of p_var | Vexpr of p_expr
and p_bloc = p_var_expr desc list
and p_class = { class_name : p_ident;
				class_type_params : p_param_type_class desc list;
				class_params : p_param desc list option;
				class_decls : p_decl desc list;
				class_extends : (p_type * (p_expr desc list option)) option
			  }
and p_prog = { prog_classes : p_class desc list;
			   prog_main : p_decl desc list
			 }
