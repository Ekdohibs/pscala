open Lexing
	   
let escape_string ff s =
  for i = 0 to String.length s - 1 do
	match s.[i] with
	| '"' -> Printf.fprintf ff "\\\""
	| '\\' -> Printf.fprintf ff "\\\\"
	| _ -> Printf.fprintf ff "%c" s.[i]
  done
  
let report_error filename start_pos end_pos =
  Printf.printf "File \"%a\", line %d, characters %d-%d:\n" escape_string filename start_pos.pos_lnum start_pos.pos_bol end_pos.pos_bol

let arg = Sys.argv.(1)
let in_chan = open_in arg
let lexbuf = Lexing.from_channel in_chan
let prog = try
	Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexing_error s ->
	 begin
	   report_error arg lexbuf.lex_start_p lexbuf.lex_curr_p;
	   print_endline s; exit 1
	 end
  | Parser.Error ->
	 begin
	   report_error arg lexbuf.lex_start_p lexbuf.lex_curr_p;
	   print_string "Syntax error\n"; exit 1
	 end
(* | _ -> print_string "no2\n"; exit 1 *)
let () = print_string "ok\n"; exit 0
