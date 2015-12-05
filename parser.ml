exception Parser_error of string

open Ast
include Menhir_parse

let check_int_bound s s2 =
  if String.length s = String.length s2 then
	s <= s2
  else
	String.length s < String.length s2

let rec walk_expr expr =
  { location = expr.location; desc =
	match expr.desc with
	| Eint i -> if check_int_bound i "2147483647" then
				  Eint i
				else
				  raise (Parser_error ("Constant too big: " ^ i))
	| Eunary (Uminus, {desc = Eint i; _}) ->
	   if check_int_bound i "2147483648" then
		 Eint ("-" ^ i)
	   else
		 raise (Parser_error ("Constant too big: " ^ ("-" ^ i)))
	| Estring _ | Ebool _ | Eunit | Ethis | Enull | Ereturn None ->
		expr.desc
	| Eaccess acc -> Eaccess (walk_acc acc)
	| Eassign (acc, e) -> Eassign (walk_acc acc, walk_expr e)
	| Ecall (acc, tt, es) ->
	   Ecall (walk_acc acc, tt, List.map walk_expr es)
	| Enew (name, tt, es) ->
	   Enew (name, tt, List.map walk_expr es)
	| Eunary (op, e) -> Eunary (op, walk_expr e)
	| Ebinary (op, e1, e2) -> Ebinary (op, walk_expr e1, walk_expr e2)
	| Eif (e1, e2, e3) -> Eif (walk_expr e1, walk_expr e2, walk_expr e3)
	| Ewhile (e1, e2) -> Ewhile (walk_expr e1, walk_expr e2)
	| Ereturn (Some e) -> Ereturn (Some (walk_expr e))
	| Eprint e -> Eprint (walk_expr e)
	| Ebloc b -> Ebloc (walk_bloc b)
  }

and walk_acc acc =
  match acc.desc with
  | Avar a -> acc
  | Afield (e, id) ->
	 { location = acc.location;
	   desc = Afield (walk_expr e, id) }

and walk_bloc b =
  { location = b.location;
	desc = 
	  List.map (function
				 | Vvar v -> Vvar (walk_var v)
				 | Vexpr e -> Vexpr (walk_expr e)
			   ) b.desc
  }

and walk_var v = 
	{ location = v.location;
	  desc = { v.desc with var_expr =
							 walk_expr v.desc.var_expr }
	}

let walk_decl = function
  | Dvar v -> Dvar (walk_var v)
  | Dmethod m ->
	 Dmethod
	   { location = m.location;
		 desc = { m.desc with
				  method_body = walk_expr m.desc.method_body }
	   }

let walk_class c =
  { location = c.location;
	desc = { c.desc with
			 class_decls = List.map walk_decl c.desc.class_decls;
			 class_extends =
			   (fst c.desc.class_extends,
				List.map walk_expr (snd c.desc.class_extends)) }
  }

let walk_prog p =
  { prog_classes = List.map walk_class p.prog_classes;
	prog_main = { location = p.prog_main.location;
				  desc = List.map walk_decl p.prog_main.desc } }
	
let prog token lexbuf =
  let p = Menhir_parse.prog token lexbuf in
  walk_prog p
