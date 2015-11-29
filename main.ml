open Lexing
	   
let escape_string ff s =
  for i = 0 to String.length s - 1 do
	match s.[i] with
	| '"' -> Printf.fprintf ff "\\\""
	| '\\' -> Printf.fprintf ff "\\\\"
	| _ -> Printf.fprintf ff "%c" s.[i]
  done
  
let report_error filename start_pos end_pos =
  let start_col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
  let end_col = end_pos.pos_cnum - start_pos.pos_bol + 1 in
  Printf.printf "File \"%a\", line %d, characters %d-%d:\n" escape_string filename start_pos.pos_lnum start_col end_col

let arg = Sys.argv.(1)
let in_chan = open_in arg
let lexbuf = Lexing.from_channel in_chan
let prog = try
	Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexing_error s | Parser_error.Parser_error s ->
	 begin
	   report_error arg (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
	   print_endline s; exit 1
	 end
  | Parser.Error ->
	 begin
	   report_error arg (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
	   print_string "Syntax error\n"; exit 1
	 end
  | _ -> print_string "Internal compiler error\n"; exit 2
(* Vérifier les arguments de ligne de commande avant de typer ! *) 
let () = Typing.type_program prog
let () = print_string "ok\n"; exit 0
