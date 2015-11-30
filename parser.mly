%{
  open Ast
  open Parser_error
  let w pos x = { location = pos; desc = x }
  let sugar x = { location = Lexing.dummy_pos, Lexing.dummy_pos; desc = x }
  let check_int_bound s s2 =
	if String.length s = String.length s2 then
	  s <= s2
	else
	  String.length s < String.length s2

%}

%token CLASS EQ EXTENDS MAIN NE NEW NULL OBJECT OVERRIDE THIS VAL VAR
%token SUBTYPE SUPERTYPE
%token DEF EQUAL PRINT RETURN
%token IF ELSE WHILE
%token DBLEQUAL UNEQ GEQ LEQ GR LW
%token OR AND
%token PLUS MINUS TIME DIV MOD
%token LEFTPAR RIGHTPAR LEFTSQBRACK RIGHTSQBRACK LEFTBRACK RIGHTBRACK
%token DOT COMMA COLON SEMICOLON BANG
%token EOF
%token <string> INT
%token <string> STRING
%token <bool> BOOL
%token <string> IDENT

%nonassoc IF
%nonassoc ELSE
%nonassoc WHILE RETURN
%right EQUAL
%left OR
%left AND
%left EQ NE DBLEQUAL UNEQ
%left GR LW GEQ LEQ
%left PLUS MINUS
%left TIME DIV MOD
%right UMINUS BANG
%left DOT

%start prog
%type <Ast.p_prog> prog
	   
(* donner le type des valeurs renvoyées par l'analyseur syntaxique *)

%%

prog:
  e = classe* ; m = classe_Main ; EOF { { prog_classes = e;
										  prog_main = m } }

classe:
  | CLASS ; i = IDENT ; lptc = square_list(param_type_classe) ;
  		lp = loption(delimited(LEFTPAR, separated_list(COMMA, parametre) , RIGHTPAR)) ;
		ic = option(int_class) ; LEFTBRACK ; 
		ld = separated_list(SEMICOLON, decl) ; RIGHTBRACK
			 { w ($startpos, $endpos)
			   { class_name = i;
				 class_type_params = lptc;
				 class_params = lp;
				 class_decls = ld;
				 class_extends =
				   match ic with
				   | None -> (sugar { type_name = "AnyRef"; arguments_type = [] }), []
				   | Some e -> e
			   } }

int_class:
  | EXTENDS ; t = type_scala ; 
  		le = loption(delimited(LEFTPAR, separated_list(COMMA, expr), RIGHTPAR))
		 { (t, le) }

decl:
  | v = var { w ($startpos, $endpos) (Dvar v) }
  | m = methode { w ($startpos, $endpos) (Dmethod m) }

var:
  | VAR ; i = IDENT ; t = option(preceded(COLON, type_scala)) ; EQUAL ; e = expr
          { w ($startpos, $endpos) { var_mutable = true;
					   var_name = i;
					   var_type = t;
					   var_expr = e } }
  | VAL ; i = IDENT ; t = option(preceded(COLON, type_scala)) ; EQUAL ; e = expr
		  { w ($startpos, $endpos) { var_mutable = false;
					   var_name = i;
					   var_type = t;
					   var_expr = e } }

square_list(X):
  | { [] }
  | LEFTSQBRACK; l = separated_nonempty_list(COMMA, X); RIGHTSQBRACK
	  { l }

methode:
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ; 
  		lpt = square_list(param_type) ;	LEFTPAR ;
		lp = separated_list(COMMA, parametre) ; RIGHTPAR ; blc = bloc
		    { w ($startpos, $endpos)
			  { method_override = b ;
			    method_name = i ;
				method_param_types = lpt ;
				method_params = lp ;
				method_type = 
				  sugar { type_name = "Unit" ; arguments_type = [] } ;
				method_body = { location = blc.location ; desc = Ebloc blc } } }
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ;
  		lpt = square_list(param_type) ;
		LEFTPAR ; lp = separated_list(COMMA, parametre) ; RIGHTPAR ;
		COLON ; t = type_scala ; EQUAL ; e = expr
		   { w ($startpos, $endpos)
		   	 { method_override = b ;
		           method_name = i ;
				   method_param_types = lpt ;
				   method_params = lp ;
				   method_type = t ;
				   method_body = e } }

parametre:
  | i = IDENT ; COLON ; t = type_scala
          { w ($startpos, $endpos) { par_name = i;
				par_type = t } }

param_type:
  | i = IDENT { w ($startpos, $endpos) { param_type_name = i;
					param_type_constraint = Any }
			  }
  | i = IDENT ; SUBTYPE ; t = type_scala
          { w ($startpos, $endpos) { param_type_name = i;
				param_type_constraint = Subtype t }
		  }
  | i = IDENT ; SUPERTYPE ; t = type_scala
          { w ($startpos, $endpos) { param_type_name = i;
				param_type_constraint = Supertype t }
		  }

param_type_classe:
  | PLUS ; p = param_type { w ($startpos, $endpos) { param_type = p;
								param_variance = Covariant
							  } }
  | MINUS ; p = param_type { w ($startpos, $endpos) { param_type = p;
								 param_variance = Contravariant
							   } }
  | p = param_type { w ($startpos, $endpos) { param_type = p;
						 param_variance = Invariant
					   } }

type_scala:
  | i = IDENT ; a = arguments_type
          { w ($startpos, $endpos) { type_name = i; arguments_type = a } }

arguments_type:
  | { [] }
  | LEFTSQBRACK ; l = separated_nonempty_list(COMMA, type_scala) ; RIGHTSQBRACK
      { l }

classe_Main:
  | OBJECT ; MAIN ; LEFTBRACK ; l = separated_list(SEMICOLON, decl) ; RIGHTBRACK 
      { l }

expr:
  | e = expr_desc { w ($startpos, $endpos) e }
	  
expr_desc:
  | i = INT { if check_int_bound i "2147483647" then
				Eint i
			  else
				raise (Parser_error ("Constant too big: " ^ i)) }
  | s = STRING { Estring s }
  | b = BOOL { Ebool b }
  | LEFTPAR ; RIGHTPAR { Eunit }
  | THIS { Ethis }
  | NULL { Enull }
  | LEFTPAR ; e = expr ; RIGHTPAR { e.desc }
  | a = acces { Eaccess a }
  | a = acces ; EQUAL ; e = expr { Eassign (a, e) }
  | a = acces ; arg = arguments_type ; LEFTPAR ;
  	    l = separated_list(COMMA, expr) ; RIGHTPAR
			{ let acc = match a.desc with
				(* Pas d'objet explicite *)
				| Avar x -> { location = a.location;
							  desc = Afield (sugar Ethis, x) }
				| _ -> a
			  in Ecall (acc, arg, l) }
  | NEW ; i = IDENT ; arg = arguments_type ; LEFTPAR ;
  	    l = separated_list(COMMA, expr) ; RIGHTPAR
		    { Enew (i, arg, l) }
  | BANG ; e = expr { Eunary (Unot, e) }
  | MINUS ; e = expr %prec UMINUS { Eunary (Uminus, e) }
  | e1 = expr ; PLUS ; e2 = expr { Ebinary (Bplus, e1, e2) }
  | e1 = expr ; MINUS ; e2 = expr { Ebinary (Bminus, e1, e2) }
  | e1 = expr ; TIME ; e2 = expr { Ebinary (Btimes, e1, e2) }
  | e1 = expr ; DIV ; e2 = expr { Ebinary (Bdiv, e1, e2) }
  | e1 = expr ; MOD ; e2 = expr { Ebinary (Bmod, e1, e2) }
  | e1 = expr ; AND ; e2 = expr { Ebinary (Band, e1, e2) }
  | e1 = expr ; OR ; e2 = expr { Ebinary (Bor, e1, e2) }
  | e1 = expr ; EQ ; e2 = expr { Ebinary (Beq, e1, e2) }
  | e1 = expr ; NE ; e2 = expr { Ebinary (Bne, e1, e2) }
  | e1 = expr ; DBLEQUAL ; e2 = expr { Ebinary (Bequal, e1, e2) }
  | e1 = expr ; UNEQ ; e2 = expr { Ebinary (Bnotequal, e1, e2) }
  | e1 = expr ; LW ; e2 = expr { Ebinary (Blt, e1, e2) }
  | e1 = expr ; GR ; e2 = expr { Ebinary (Bgt, e1, e2) }
  | e1 = expr ; LEQ ; e2 = expr { Ebinary (Ble, e1, e2) }
  | e1 = expr ; GEQ ; e2 = expr { Ebinary (Bge, e1, e2) }
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr %prec IF
        { Eif (e1, e2, sugar Eunit) }
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr ; ELSE ; e3 = expr %prec IF
		{ Eif (e1, e2, e3) }
  | WHILE ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr %prec WHILE
		{ Ewhile (e1, e2) }
  | RETURN ; e = ioption(expr) %prec RETURN
		{ Ereturn e }
  | PRINT ; LEFTPAR ; e = expr ; RIGHTPAR
		{ Eprint e }
  | b = bloc { Ebloc b }

bloc:
  | LEFTBRACK ; l = separated_list(SEMICOLON, int_bloc) ; RIGHTBRACK
  		{ w ($startpos, $endpos) l }

int_bloc:
  | v = var   { (Vvar v) }
  | e = expr  { (Vexpr e) }

acces:
  | i = IDENT { w ($startpos, $endpos) (Avar i) }
  | e = expr ; DOT ; i = IDENT { w ($startpos, $endpos) (Afield (e, i)) }
