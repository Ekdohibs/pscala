
type reg =
  | Rax | Rbx | Rcx | Rdx | Rdi | Rsi | Rbp | Rsp
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let reg_to_string = function
  | Rax -> "rax" | Rbx -> "rbx" | Rcx -> "rcx" | Rdx -> "rdx"
  | Rdi -> "rdi" | Rsi -> "rsi" | Rbp -> "rbp" | Rsp -> "rsp"
  | R8  -> "r8"  | R9  -> "r9"  | R10 -> "r10" | R11 -> "r11"
  | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"

let reg_to_int = function
  | Rax ->  0 | Rbx ->  1 | Rcx ->  2 | Rdx ->  3
  | Rdi ->  4 | Rsi ->  5 | Rbp ->  6 | Rsp -> -1
  | R8  ->  7 | R9  ->  8 | R10 ->  9 | R11 -> 10
  | R12 -> 11 | R13 -> 12 | R14 -> 13 | R15 -> 14
														  
type t =
	Pseudo of int
  | Reg of reg

let fresh = begin
	let index = ref 0 in
	(fun () ->
	  incr index;
	  Pseudo !index)
  end

let rax = Reg Rax
let rbx = Reg Rbx
let rcx = Reg Rcx
let rdx = Reg Rdx
let rdi = Reg Rdi
let rsi = Reg Rsi
let rbp = Reg Rbp
let rsp = Reg Rsp
let r8 = Reg R8
let r9 = Reg R9
let r10 = Reg R10
let r11 = Reg R11
let r12 = Reg R12
let r13 = Reg R13
let r14 = Reg R14
let r15 = Reg R15
let parameters = [rdi; rsi; rdx; rcx; r8; r9]
let callee_saved = [rbx; rbp; r12; r13; r14; r15]
let caller_saved = [rax; r10; r11] @ parameters
			  
let compare = compare

let print ff r =
  match r with
	Pseudo n -> Format.fprintf ff "#%d" n
  | Reg r -> Format.fprintf ff "%%%s" (reg_to_string r)

let is_real = function
  | Pseudo _ -> false
  | _ -> true
