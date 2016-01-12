
module Smap = Map.Make(String)
module Sset = Set.Make(String)

type xunary =
  | Xneg | Xnot
  | Xeqi of int64 | Xnei of int64 | Xlti of int64
  | Xlei of int64 | Xgti of int64 | Xgei of int64
  | Xaddi of int64 | Xmuli of int64
  | Xdivi of int64 | Xmodi of int64
  | Xsli of int | Xasri of int | Xlsri of int

type xbinary =
  | Xeq | Xne | Xlt | Xle | Xgt | Xge
  | Xadd | Xsub | Xrsub | Xmul | Xdiv | Xmod
  | Xsl | Xasr | Xlsr
					  
type expr =
  | Eint of int64
  | Estring of string
  | Eunit
  | Egetlocal of int
  | Esetlocal of int * expr
  | Egetfield of expr * int
  | Esetfield of expr * int * expr
  | Ecall of string * expr list
    (* offset, args *) 
  | Ecallmethod of int * expr list
    (* header, size in words (header included) *)
  | Eallocbloc of string * int
  | Eunary of xunary * expr
  | Ebinary of xbinary * expr * expr
  | Eand of expr * expr
  | Eor of expr * expr
  | Eif of expr * expr * expr
  | Ewhile of expr * expr
  | Ereturn of expr
  | Eprintint of expr
  | Eprintstring of expr
  | Ebloc of expr list

type fundef = {
  fun_name : string;
  fun_params : int;
  fun_body : expr;
  fun_has_value : bool;
}

type program = {
  prog_functions : fundef list;
  prog_class_descrs : (string * string * string list) list;
}
				  
