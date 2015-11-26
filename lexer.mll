{
  open Parser
  open Lexing
	exception Lexing_error of string

	let kw =
		[ "class", CLASS ;
		  "def", DEF ;
		  "else", ELSE ;
		  "eq", EQ ;
		  "extends", EXTENDS ;
		  "false", BOOL false ;
		  "if", IF ;
		  "Main", MAIN ;
		  "ne", NE ;
		  "new", NEW ;
		  "null", NULL ;
		  "object", OBJECT ;
		  "override", OVERRIDE ;
		  "print", PRINT ;
		  "return", RETURN ;
		  "this", THIS ;
		  "true", BOOL true ;
		  "val", VAL ;
		  "var", VAR ;
		  "while", WHILE ]
	
	let keywords = Hashtbl.create (List.length kw)
	let () = List.iter (fun (a,b) -> Hashtbl.add keywords a b) kw

	let newline lexbuf =
	  let pos = lexbuf.lex_curr_p in
	  lexbuf.lex_curr_p <- 
	    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let digits = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let car = "\\\\" | "\\\"" | "\\n" | "\\t"  | [' ' '!' ] | [ '#'-'['] | [ ']'-'~' ]
let mot = alpha ( alpha | digits | '_' )*
let whitespace =  [ ' ' '\t' ]

rule token = parse
  | whitespace					{ token lexbuf }
  | "//"						{ comment_line lexbuf }
  | "(*"						{ comment_base lexbuf }
  | "\n"						{ newline lexbuf ; token lexbuf }
  | "=="						{ DBLEQUAL }
  | "="							{ EQUAL }
  | "||"						{ OR }
  | "&&"						{ AND }
  | "!="						{ UNEQ }
  | ">="						{ GEQ }
  | "<="						{ LEQ }
  | "<:"						{ SUBTYPE }
  | ">:"						{ SUPERTYPE }
  | ">"							{ GR }
  | "<"							{ LW }
  | "+"							{ PLUS }
  | "-"							{ MINUS }
  | "*"							{ TIME }
  | "/"							{ DIV }
  | "%"							{ MOD }
  | "."							{ DOT }
  | eof							{ EOF }
  | "("							{ LEFTPAR }
  | ")"							{ RIGHTPAR }
  | "["							{ LEFTSQBRACK }
  | "]"							{ RIGHTSQBRACK }
  | "{"							{ LEFTBRACK }
  | "}"							{ RIGHTBRACK }
  | "\"" car* as s "\"" 		{ STRING s }
  | ":"							{ COLON }
  | ";"							{ SEMICOLON }
  | ","							{ COMMA }
  | "!"							{ BANG }
  | mot	as s					{ try Hashtbl.find keywords s
     with Not_found -> IDENT s }
  | digits+ as s				{ (* TODO: la constante doit etre entre -2^31 et 2^31 - 1 *) INT (int_of_string s) }
  | _ 		   					{ raise (Lexing_error "Illegal caracter") }

and comment_line = parse
  | "\n"	{ newline lexbuf; token lexbuf }
  | _		{ comment_line lexbuf }
  | eof		{ EOF }

and comment_base = parse
  | "*)"			{ token lexbuf }
  | _				{ comment_base lexbuf }
  | eof				{ raise (Lexing_error "Commentaire non terminé") }

