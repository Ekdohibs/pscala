{

	exception Lexing_error of string

	let kw =
		[ "class", CLASS ;
		  "def", DEF ;
		  "else", ELSE ;
		  "eq", EQ ;
		  "extends", EXTENDS ;
		  "false", FALSE ;
		  "if", IF ;
		  "ne", NE ;
		  "new", NEW ;
		  "null", NULL ;
		  "object", OBJECT ;
		  "override", OVERRIDE ;
		  "print", PRINT ;
		  "return", RETURN ;
		  "this", THIS ;
		  "true", TRUE ;
		  "val", VAL ;
		  "var", VAR ;
		  "while", WHILE ]
	
	let keywords = Hashtbl.create (List.length kw)
	let () = List.iter (fun (a,b) -> Hashtbl.add keywords a b) kw

	let newline lexbuf =
	  let pos = lexbuf.lex_curr_p in
	  lexbuf.lex_curr_p <- 
	    { pos with pos_lnum = pos.po_lnum + 1; pos_bol = pos.pos_cnum }

}

let digits = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let car = [ '\\' '\n' '\t' ' ' '!' ] | [ '#'-'['] | [ ']'-'~' ]
let mot = ['a'-'z'] ( alpha | digits | '_' )*
let whitespace =  [ ' ' '\t' ]

rule token = parse
  | whitespace			{ token lexbuf }
  | "//"				{ comment_line lexbuf }
  | "(*"				{ comment_base lexbuf }
  | "\n"				{ newline lexbuf ; token lexbuf }
  | "=="				{ DBLEQUAL }
  | "="					{ EQUAL }
  | "||"				{ OR }
  | "&&"				{ AND }
  | "!="				{ UNEQ }
  | ">="				{ GEQ }
  | "<="				{ LEQ }
  | ">"					{ GR }
  | "<"					{ LW }
  | "+"					{ PLUS }
  | "-"					{ MINUS }
  | "*"					{ TIME }
  | "/"					{ DIV }
  | "%"					{ MOD }
  | "."					{ DOT }
  | eof					{ EOF }
  | "("					{ LEFTPAR }
  | ")"					{ RIGHTPAR }
  | "["					{ LEFTCROCH }
  | "]"					{ RIGHTCROCH }
  | '"' car* as s '"'	{ STRING s }
  | '\'' car as s '\''	{ CHAR s }
  | mot	as s			{ try Hashtbl.find keywords s with Not_found -> VAR s }
  | digits+ as s		{ INT (int_of_string s) }

and comment_line = parse
  | "\n"	{ token }
  | _		{ comment_line lexbuf }
  | eof		{ EOF }

and comment_base = parse
  | "*)"			{ token lexbuf }
  | _				{ comment_base lexbuf }
  | eof				{ raise (Lexing_error "commentaire non terminé") }
