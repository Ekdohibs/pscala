{

}

%token EOF CLASS DEF EQ EXTENDS NE NEW NULL OBJECT OVERRIDE
%token IF ELSE WHILE
%token PRINT RETURN THIS VAL
%token DBLEQUAL EQUAL UNEQ GEQ LEQ GR LW
%token OR AND FALSE TRUE
%token LEFTPAR RIGHTPAR LEFTCROCH RIGHTCROCH
%token <int> INT
%token <string> VAR
%token <string> STRING
%token <char> CHAR

(* if *)
(* while et return *)
%right EQUAL
%left OR
%left AND
%left EQ NE DBLEQUAL UNEQ
%left GR LW GEQ LEQ
%left PLUS MINUS
%left TIME DIV MOD
%right (* - unaire et ! *)
%left DOT

%start prog

(* donner le type des valeurs renvoyées par l'analyseur syntaxique *)

%%

prog:
  e = classe* ; classe_Main ; EOF
;

classe:
  | CLASS ; v = VAR
  | c = classe ; LEFTCROCH ; p = mult_param_type_class ; RIGHTCROCH
  | c = classe ; LEFTPAR ; p = mult_param ; RIGHTPAR
(* à compléter *)


decl:
  | v = VAR
  | m = methode

var 
