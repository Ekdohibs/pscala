{

  open Ast

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
  | CLASS ; i = IDENT ; lptc = option(LEFTSQBRACK ; 
  		separated_nonempty_list(COMMA, param_type_classe) ; RIGHTBRACK) ;
  		lp = option(LEFTPAR ; separated_list(COMMA, parametre) ; RIGHTPAR) ;
		ic = option(int_class) ; LEFTBRACK ; 
		ld = separated_list(SEMICOLON, decl) ; RIGHTBRACK

int_classe:
  | EXTENDS ; t = type_scala ; 
  		le = option(LEFTPAR ; separated_list(COMMA, expr) ; RIGHTPAR)

decl:
  | v = VAR
  | m = methode

var:
  | VAR ; i = IDENT ; t = option( COLON ; type_scala ) ; EQUAL ; e = expr
  | VAL ; i = IDENT ; t = option( COLON ; type_scala ) ; EQUAL ; e = expr

methode:
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ; 
  		lpt = option(RIGHTSQBRACK ; nonempty_separated_list(COMMA, param_type) ;
		LEFTSQBRACK) ;
		LEFTPAR ; lp = separated_list(COMMA, parametre) ; RIGHTPAR ; blc = bloc
  | b = boption(OVERRIDE) ; DEF ; i = IDENT ;
  		lpt = option(RIGHTSQBRACK ; nonempty_separated_list(COMMA, param_type) ;
		LEFTSQBARCK) ;
		LEFTPAR ; lp = separated_list(COMMA, parametre) ; RIGHTPAR ;
		COLON ; t = type_scala ; EQUAL ; e = expr

parametre:
  | i = IDENT ; COLON ; t = type_scala

param_type:
  | i = IDENT
  | i = IDENT ; SUBTYPE ; t = type_scala
  | i = IDENT ; SUPERTYPE ; t = type_scala

param_type_classe:
  | PLUS ; param_type
  | MINUS ; param_type
  | p = param_type

type_scala:
  | i = IDENT ; a = arguments_type

arguments_type:
  | 
  | LEFTSQBRACK ; l = separated_nonempty_list(COMMA, type_scala) ; RIGHTSQBRACK

classe_Main:
  | OBJECT ; MAIN ; LEFTBRACK ; l = separated_list(SEMICOLON, decl) ; RIGHTBRACK

expr:
  | i = INT
  | s = STRING
  | b = BOOL
  | LEFTPAR ; RIGHTPAR
  | THIS
  | NULL
  | LEFTPAR ; e = expr ; RIGHTPAR
  | a = acces
  | a = acces ; EQUAL ; e = expr
  | a = acces ; arg = arguments_type ; LEFTPAR ;
  		l = separated_list(COMMA, expr) ; RIGHTPAR
  | NEW ; i = IDENT ; arg = arguments_type ; LEFTPAR ;
  		l = separated_list(COMMA, expr) ; RIGHTPAR
  | BANG ; e = expr
  | MINUS ; e = expr
  | e1 = expr ; PLUS ; e2 = expr
  | e1 = expr ; MINUS ; e2 = expr
  | e1 = expr ; TIME ; e2 = expr
  | e1 = expr ; DIV ; e2 = expr
  | e1 = expr ; MOD ; e2 = expr
  | e1 = expr ; AND ; e2 = expr
  | e1 = expr ; OR ; e2 = expr
  | e1 = expr ; EQ ; e2 = expr
  | e1 = expr ; NE ; e2 = expr
  | e1 = expr ; DBLEQUAL ; e2 = expr
  | e1 = expr ; NEQ ; e2 = expr
  | e1 = expr ; LW ; e2 = expr
  | e1 = expr ; GR ; e2 = expr
  | e1 = expr ; LEQ ; e2 = expr
  | e1 = expr ; GEQ ; e2 = expr
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr
  | IF ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr ; ELSE ; e3 = expr
  | WHILE ; LEFTPAR ; e1 = expr ; RIGHTPAR ; e2 = expr
  | RETURN ; e = option(expr)
  | PRINT ; LEFTPAR ; e = expr ; RIGHTPAR
  | b = bloc

bloc:
  | LEFTBRACK ; l = separated_list(SEMICOLON, int_bloc) ; RIGHTBRACK

int_bloc:
  | v = var
  | e = expr

acces:
  | i = IDENT
  | e = expr ; DOT ; i = IDENT
