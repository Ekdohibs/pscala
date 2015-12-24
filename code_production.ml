open Type_ast
open X86_64
	   
let produce_code prog = {
  text = glabel "main" ++
		 xorq (reg rax) (reg rax) ++
		 ret
  ;
  data = nop }
