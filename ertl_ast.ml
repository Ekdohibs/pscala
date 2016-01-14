
module Xops = struct
type xunary =
  | Xneg | Xnot
  | Xaddi of int32 | Xmuli of int32
  | Xdivi of int32
  | Xsli of int | Xasri of int | Xlsri of int

type xbinary =
  | Xadd | Xsub | Xmul | Xdiv
  | Xsl | Xasr | Xlsr | Xmov

type xcbinary =
  | Xcadd | Xcmul

type ubranch =
  | Ujeqi of int32 | Ujnei of int32 | Ujlti of int32
  | Ujlei of int32 | Ujgti of int32 | Ujgei of int32
  | Ujz | Ujnz

type bbranch =
  | Bjeq | Bjne | Bjlt | Bjle | Bjgt | Bjge

let print_xunary ff = function
  | Xneg -> Format.fprintf ff "neg"
  | Xnot -> Format.fprintf ff "not"
  | Xaddi n -> Format.fprintf ff "addi $%ld" n
  | Xmuli n -> Format.fprintf ff "muli $%ld" n
  | Xdivi n -> Format.fprintf ff "divi $%ld" n
  | Xsli n -> Format.fprintf ff "sli $%d" n
  | Xasri n -> Format.fprintf ff "asri $%d" n
  | Xlsri n -> Format.fprintf ff "lsri $%d" n

let print_xbinary ff b =
  Format.fprintf ff "%s" (match b with
	| Xadd -> "add" | Xsub -> "sub" | Xmul -> "imul"
	| Xdiv -> "idiv" | Xsl -> "sl"
	| Xasr -> "asr" | Xlsr -> "lsr" | Xmov -> "mov")

let print_ubranch ff (b, f) = match b with
  | Ujeqi n -> Format.fprintf ff "eqi %t $%ld" f n
  | Ujnei n -> Format.fprintf ff "nei %t $%ld" f n
  | Ujlti n -> Format.fprintf ff "lti %t $%ld" f n
  | Ujlei n -> Format.fprintf ff "lei %t $%ld" f n
  | Ujgti n -> Format.fprintf ff "gti %t $%ld" f n
  | Ujgei n -> Format.fprintf ff "gei %t $%ld" f n
  | Ujz -> Format.fprintf ff "z %t" f
  | Ujnz -> Format.fprintf ff "nz %t" f

let print_bbranch ff b =
  Format.fprintf ff "%s" (match b with
  | Bjeq -> "eq" | Bjne -> "ne" | Bjlt -> "lt"
  | Bjle -> "le" | Bjgt -> "gt" | Bjge -> "ge")										 
end
include Xops				


module LMap = Map.Make(Label)
module Rmap = Map.Make(Register)					  
module Rset = Set.Make(Register)
type label = Label.t
type register = Register.t
			   
type instr =
  | Eint of int64 * register * label
  | Estring of string * register * label
  | Eunit of register * label
  | Egetfield of register * int * register * label
  | Esetfield of register * int * register * label
  | Ecall of string * int * label
  | Ecallmethod of int * int * label
  | Esetheader of string * register * label
  | Eunary of xunary * register * label
  | Ebinary of xbinary * register * register * label
  | Ebinary3 of xcbinary * register * register * register * label
  | Ecqto of label
  | Egoto of label
  | Eubranch of ubranch * register * label * label
  | Ebbranch of bbranch * register * register * label * label
   (* Set register to 0 or 1 according to condition  *)
  | Euset of ubranch * register * register * label
  | Ebset of bbranch * register * register * register * label
  | Ealloc_frame of label
  | Edelete_frame of label
  | Eget_param of int * register * label
  | Epush_param of register * label
  | Estack_free of int * label
  | Ereturn

type graph = instr LMap.t

type fundef = {
  fun_name : string;
  fun_params : int;
  fun_entry : label;
  fun_body : graph
}

type program = {
  prog_functions : fundef list;
  prog_class_descrs : (string * string * string list) list;
}

