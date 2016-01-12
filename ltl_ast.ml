
include Ertl_ast.Xops
type color = Coloring.color
type register = Register.reg
type label = Label.t
module LMap = Map.Make(Label)
module Rmap = Map.Make(Register)
				  
type instr =
  | Eint of int32 * color * label
  | Eint64 of int64 * register * label
  | Estring of string * color * label
  | Egetfield of register * int * register * label
  | Esetfield of register * int * register * label
  | Ecall of string * label
  | Ecallmethod of int * label
  | Esetheader of string * register * label
  | Eunary of xunary * color * label
  | Ebinary of xbinary * color * color * label
  | Ecqto of label
  | Egoto of label
  | Eubranch of ubranch * color * label * label
  | Ebbranch of bbranch * color * color * label * label
  | Euset of ubranch * color * color * label
  | Ebset of bbranch * color * color * color * label
  | Epush_param of color * label
  | Ereturn

type graph = instr LMap.t
	  
type fundef = {
  fun_name : string;
  fun_entry : label;
  fun_body : graph
}
				
type program = {
  prog_functions : fundef list;
  prog_class_descrs : (string * string * string list) list;
}

