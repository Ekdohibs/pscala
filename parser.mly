{
  open Ast
  let w x = { location = Lexing.position; desc = x }
  let sugar x = { location = Lexing.dummy_pos; desc = x }
}

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
%token <int> INT
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL
%token <string> IDENT

%nonassoc IF
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

(* donner le type des valeurs renvoyées par l'analyseur syntaxique *)

%%

prog:
  e = classe* ; classe_Main ; EOF
;

classe:
  | CLASS ; i = IDENT ; lptc = square_list(param_type_classe) ;
  		lp = option(LEFTPAR ; separated_list(COMMA, parametre) ; RIGHTPAR) ;
		ic = option(int_class) ; LEFTBRACK ; 
		ld = separated_list(SEMICOLON, decl) ; RIGHTBRACK

int_classe:
  | EXTENDS ; t = type_scala ; 
  		le = option(LEFTPAR ; separated_list(COMMA, expr) ; RIGHTPAR)

decl:
  | v = var { w (desc = Dvar v) }
  | m = methode { w (Dmethod m) }

var:
  | VAR ; i = IDENT ; t = option( COLON ; type_scala ) ; EQUAL ; e = expr
          { w { var_mutable = true;
					   var_name = i;
					   var_type = t;
					   var_expr = e } }
  | VAL ; i = IDENT ; t = option( COLON ; type_scala ) ; EQUAL ; e = expr
		  { w { var_mutable = false;
					   var_name = i;
					   var_type = t;
					   var_expr = e } }

square_list(X):
  | { [] }
  | LEFTSQBRACK; l = nonempty_separated_list(COMMA, X); RIGHTSQBRACK
	  { l }

methode:
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ; 
  		lpt = square_list(param_type) ;	LEFTPAR ;
		lp = separated_list(COMMA, parametre) ; RIGHTPAR ; blc = bloc
		    { w { method_override = b ;
			      method_name = i ;
				  method_param_types = lpt ;
				  method_params = lp ;
				  method_type = 
					  sugar { type_name = "Unit" ; arguments_type = [] } ;
				  method_body = { position = blc.location ; desc = Ebloc blc } }
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ;
  		lpt = square_list(param_type) ;
		LEFTPAR ; lp = separated_list(COMMA, parametre) ; RIGHTPAR ;
		COLON ; t = type_scala ; EQUAL ; e = expr
		   { w ( { method_override = b ;
		           method_name = i ;
				   method_param_types = lpt ;
				   method_params = lp ;
				   method_type = t ;
				   method_body = e } ) }

parametre:
  | i = IDENT ; COLON ; t = type_scala
          { w { par_name = i;
				par_type = t } }

param_type:
  | i = IDENT { w { param_type_name = i;
					param_type_constraint = Any }
			  }
  | i = IDENT ; SUBTYPE ; t = type_scala {
          { w { param_type_name = i;
				param_type_constraint = Subtype t }
		  }
  | i = IDENT ; SUPERTYPE ; t = type_scala
          { w { param_type_name = i;
				param_type_constraint = Supertype t }
		  }

param_type_classe:
  | PLUS ; p = param_type { w { param_type = p;
								param_variance = Covariant
							  } }
  | MINUS ; p = param_type { w { param_type = p;
								 param_variance = Contravariant
							   } }
  | p = param_type { w { param_type = p;
						 param_variance = Invariant
					   } }

type_scala:
  | i = IDENT ; a = arguments_type {
          { w { type_name = i; arguments_type = a } }

arguments_type:
  | { [] }
  | LEFTSQBRACK ; l = separated_nonempty_list(COMMA, type_scala) ; RIGHTSQBRACK
      { l }

classe_Main:
  | OBJECT ; MAIN ; LEFTBRACK ; l = separated_list(SEMICOLON, decl) ; RIGHTBRACK 
      { l }

expr:
  | i = INT { w (Eint i) }
  | s = STRING { w (Estring s) }
  | b = BOOL { w (Ebool b) }
  | LEFTPAR ; RIGHTPAR { w Eunit }
  | THIS { w Ethis }
  | NULL { w Enull }
  | LEFTPAR ; e = expr ; RIGHTPAR { e }
  | a = acces { w (Eaccess a) }
  | a = acces ; EQUAL ; e = expr { w (Eassign (a, e)) }
  | a = acces ; arg = arguments_type ; LEFTPAR ;
  	    l = separated_list(COMMA, expr) ; RIGHTPAR
			{ let acc = match a.desc with
				(* Pas d'objet explicite *)
				| Avar x -> { location = a.location;
							  desc = Afield (sugar Ethis, x) }
				| _ -> a
			  in w (Ecall (acc, arg, l)) }
  | NEW ; i = IDENT ; arg = arguments_type ; LEFTPAR ;
  	    l = separated_list(COMMA, expr) ; RIGHTPAR
		    { w (Enew (i, arg, l)) }
  | BANG ; e = expr { w (Eunary (Unot, e)) }
  | MINUS ; e = expr { w (Eunary (Uminus, e)) }
  | e1 = expr ; PLUS ; e2 = expr { w (Ebinary (Bplus, e1, e2)) }
  | e1 = expr ; MINUS ; e2 = expr { w (Ebinary (Bminus, e1, e2)) }
  | e1 = expr ; TIME ; e2 = expr { w (Ebinary (Btimes, e1, e2)) }
  | e1 = expr ; DIV ; e2 = expr { w (Ebinary (Bdiv, e1, e2)) }
  | e1 = expr ; MOD ; e2 = expr { w (Ebinary (Bmod, e1, e2)) }
  | e1 = expr ; AND ; e2 = expr { w (Ebinary (Band, e1, e2)) }
  | e1 = expr ; OR ; e2 = expr { w (Ebinary (Bor, e1, e2)) }
  | e1 = expr ; EQ ; e2 = expr { w (Ebinary (Beq, e1, e2)) }
  | e1 = expr ; NE ; e2 = expr { w (Ebinary (Bne, e1, e2)) }
  | e1 = expr ; DBLEQUAL ; e2 = expr { w (Ebinary (Bequal, e1, e2)) }
  | e1 = expr ; NEQ ; e2 = expr { w (Ebinary (Bnotequal, e1, e2)) }
  | e1 = expr ; LW ; e2 = expr { w (Ebinary (Blt, e1, e2)) }
  | e1 = expr ; GR ; e2 = expr { w (Ebinary (Bgt, e1, e2)) }
  | e1 = expr ; LEQ ; e2 = expr { w (Ebinary (Ble, e1, e2)) }
  | e1 = expr ; GEQ ; e2 = expr { w (Ebinary (Bge, e1, e2)) }
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr
        { w (Eif (e1, e2, sugar Eunit)) }
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr ; ELSE ; e3 = expr
		{ w (Eif (e1, e2, e3)) }
  | WHILE ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr
		{ w (Ewhile (e1, e2)) }
  | RETURN ; e = option(expr)
		{ w (Ereturn e) }
  | PRINT ; LEFTPAR ; e = expr ; RIGHTPAR
		{ w (Eprint e) }
  | b = bloc { w (Ebloc b) }

bloc:
  | LEFTBRACK ; l = separated_list(SEMICOLON, int_bloc) ; RIGHTBRACK
  		{ w (l) }

int_bloc:
  | v = var   { w (Vvar v) }
  | e = expr  { w (Vexpr e) }

acces:
  | i = IDENT { w (Avar i) }
  | e = expr ; DOT ; i = IDENT { w (Afield (e, i)) }
