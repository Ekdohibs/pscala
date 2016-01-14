
type xunary =
  | Xneg | Xnot
  | Xaddi of int64 | Xmuli of int64
  | Xdivi of int64 | Xmodi of int64
  | Xsli of int | Xasri of int | Xlsri of int

type xbinary =
  | Xadd | Xsub | Xmul | Xdiv | Xmod
  | Xsl | Xasr | Xlsr | Xmov

type xcbinary =
  | Xcadd | Xcmul
						  
type ubranch =
  | Ujeqi of int64 | Ujnei of int64 | Ujlti of int64
  | Ujlei of int64 | Ujgti of int64 | Ujgei of int64
  | Ujz | Ujnz

type bbranch =
  | Bjeq | Bjne | Bjlt | Bjle | Bjgt | Bjge
				 
module LMap = Map.Make(Label)
module Rset = Set.Make(Register)
type label = Label.t
type register = Register.t
			   
type instr =
  | Eint of int64 * register * label
  | Estring of string * register * label
  | Eunit of register * label
  | Egetfield of register * int * register * label
  | Esetfield of register * int * register * label
  | Ecall of register * string * register list * label
  | Ecallmethod of register * int * register list * label
  | Eallocbloc of string * int * register * label
  | Eunary of xunary * register * label
  | Ebinary of xbinary * register * register * label
  | Ebinary3 of xcbinary * register * register * register * label
  | Eprintint of register * label
  | Eprintstring of register * label
  | Egoto of label
  | Eubranch of ubranch * register * label * label
  | Ebbranch of bbranch * register * register * label * label
   (* Set register to 0 or 1 according to condition  *)
  | Euset of ubranch * register * register * label
  | Ebset of bbranch * register * register * register * label

type graph = instr LMap.t

type fundef = {
  fun_name : string;
  fun_params : register list;
  fun_result : register;
  fun_entry : label;
  fun_exit : label;
  fun_body : graph;
  fun_has_value : bool
}

type program = {
  prog_functions : fundef list;
  prog_class_descrs : (string * string * string list) list;
}

