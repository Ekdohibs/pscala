open Lexing
open Format
	   
let usage = "usage: pscala [options] file.scala"
let parse_only = ref false
let type_only = ref false
let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
	"--type-only", Arg.Set type_only, "  stop after typing";
	"-G", Arg.Set Debug.enable_debug, "  show debug messages";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".scala") then
      raise (Arg.Bad "Input file should have a .scala extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1
	   
let escape_string ff s =
  for i = 0 to String.length s - 1 do
	match s.[i] with
	| '"' -> fprintf ff "\\\""
	| '\\' -> fprintf ff "\\\\"
	| _ -> fprintf ff "%c" s.[i]
  done
  
let report_error filename start_pos end_pos =
  let start_col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
  let end_col = end_pos.pos_cnum - start_pos.pos_bol + 1 in
  eprintf "File \"%a\", line %d, characters %d-%d:\n" escape_string filename start_pos.pos_lnum start_col end_col
		  
let in_chan = open_in file
let lexbuf = Lexing.from_channel in_chan
let prog = Debug.protect begin fun () ->
  try
	Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexing_error s | Parser.Parser_error s ->
	 begin
	   report_error file (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
	   eprintf "%s@." s; exit 1
	 end
  | Parser.Error ->
	 begin
	   report_error file (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf);
	   eprintf "Syntax error@."; exit 1
	 end
end
let () = if !parse_only then exit 0
let decorated = Debug.protect begin fun () ->
  try
	Typing.type_program prog
  with
  | Typing.Typing_error (e, (startpos, endpos)) ->
	 begin
	   report_error file startpos endpos;
	   eprintf "Typing error:@ %t@." e;
	   exit 1
	 end
end
let () = if !type_only then exit 0
let asm = Debug.protect begin fun () ->
  (* Code_production.produce_code decorated *)
  let is = Is.program decorated in
  let rtl = Rtl.program is in
  let ertl = Ertl.program rtl in
  let ltl = Ltl.program ertl in
  Lin.program ltl
end
let asm_filename = (Filename.chop_suffix file ".scala") ^ ".s"
let () = X86_64.print_in_file asm_filename asm
let () = exit 0
