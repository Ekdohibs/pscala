
let filename = "tests/exec/fact_rec.scala";;
let in_chan = open_in filename;;
let prog = Parser.prog Lexer.token (Lexing.from_channel in_chan);;
let decorated = Typing.type_program prog;;
let is = Is.program decorated;;
let rtl = Rtl.program is;;
Rtl.print_program Format.std_formatter rtl;;
let ertl = Ertl.program rtl;;
(* Liveliness.print_program Format.std_formatter ertl;; *)
let ltl= Ltl.program ertl;;
Ltl.print_program Format.std_formatter ltl;;
