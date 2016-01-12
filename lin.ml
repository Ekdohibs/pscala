
include Ertl_ast.Xops
type label = Label.t
module LMap = Map.Make(Label)
open X86_64
					  
let needed_labels = Hashtbl.create 17
let code = ref []
let string_index = ref 0
let make_string_label () =
  incr string_index;
  "string_" ^ (string_of_int !string_index)
let seen_strings = Hashtbl.create 17
let add_string s =
  try Hashtbl.find seen_strings s
  with Not_found -> let l = make_string_label () in
					Hashtbl.add seen_strings s l;
					l
let descr_label c = "D_" ^ c
let visited = Hashtbl.create 17
			   
let emit l text = code := (l, text) :: !code
let need_label l = Hashtbl.add needed_labels l ()

let get_code () =
  let c = ref nop in
  List.iter (fun (l, t) ->
	c := (if Hashtbl.mem needed_labels l then
			label (Label.to_string l) ++ t
		  else
			t) ++ !c) !code;
  code := [];
  Hashtbl.clear needed_labels;
  Hashtbl.clear visited;
  !c

let get_strings () =
  let c = ref nop in
  Hashtbl.iter (fun s l -> c := !c ++ (label l) ++
								      (string_unescaped s)) seen_strings;
  Hashtbl.clear seen_strings;
  !c

let class_descrs d =
  List.fold_left (++) nop (List.map
	(fun (class_name, parent_name, l) ->
	   label (descr_label class_name) ++
	   address [descr_label parent_name] ++
	   address l) d)
   
let reg_to_q = function
  | Register.Rax -> rax | Register.Rbx -> rbx
  | Register.Rcx -> rcx | Register.Rdx -> rdx
  | Register.Rsi -> rsi | Register.Rdi -> rdi
  | Register.Rbp -> rbp | Register.Rsp -> rsp
  | Register.R8  -> r8  | Register.R9  -> r9
  | Register.R10 -> r10 | Register.R11 -> r11
  | Register.R12 -> r12 | Register.R13 -> r13
  | Register.R14 -> r14 | Register.R15 -> r15

let reg_to_b = function
  | Register.Rax -> al   | Register.Rbx -> bl
  | Register.Rcx -> cl   | Register.Rdx -> dl
  | Register.Rsi -> sil  | Register.Rdi -> dil
  | Register.Rbp -> bpl  | Register.Rsp -> spl
  | Register.R8  -> r8b  | Register.R9  -> r9b
  | Register.R10 -> r10b | Register.R11 -> r11b
  | Register.R12 -> r12b | Register.R13 -> r13b
  | Register.R14 -> r14b | Register.R15 -> r15b
											
let word_size = 8
let operand = function
  | Coloring.Reg r -> reg (reg_to_q r)
  | Coloring.Spilled n -> ind ~ofs:(word_size * n) rsp

let operand_b = function
  | Coloring.Reg r -> reg (reg_to_b r)
  | Coloring.Spilled n -> ind ~ofs:(word_size * n) rsp
							  
let zero r = match r with
  | Coloring.Reg _ -> xorq (operand r) (operand r)
  | Coloring.Spilled _ -> movq (imm32 0l) (operand r)
							  
let test r = match r with
  | Coloring.Reg _ -> testq (operand r) (operand r)
  | Coloring.Spilled _ -> cmpq (imm32 0l) (operand r)

let ubranch ub r l =
  match ub with
  | Ujeqi n -> cmpq (imm32 n) (operand r) ++ je l
  | Ujnei n -> cmpq (imm32 n) (operand r) ++ jne l
  | Ujlti n -> cmpq (imm32 n) (operand r) ++ jl l
  | Ujlei n -> cmpq (imm32 n) (operand r) ++ jle l
  | Ujgti n -> cmpq (imm32 n) (operand r) ++ jg l
  | Ujgei n -> cmpq (imm32 n) (operand r) ++ jge l
  | Ujz     -> test r ++ jz l
  | Ujnz    -> test r ++ jnz l

let inv_ubranch = function
  | Ujeqi n -> Ujnei n | Ujnei n -> Ujeqi n
  | Ujlti n -> Ujgei n | Ujgei n -> Ujlti n
  | Ujlei n -> Ujgti n | Ujgti n -> Ujlei n
  | Ujz     -> Ujnz    | Ujnz    -> Ujz

let bbranch bb r1 r2 l =
  cmpq (operand r2) (operand r1) ++
  match bb with
  | Bjeq -> je l
  | Bjne -> jne l
  | Bjlt -> jl l
  | Bjle -> jle l
  | Bjgt -> jg l
  | Bjge -> jge l

let inv_bbranch = function
  | Bjeq -> Bjne | Bjne -> Bjeq
  | Bjlt -> Bjge | Bjge -> Bjlt
  | Bjle -> Bjgt | Bjgt -> Bjle

let uset ub r1 r2 =
  (match ub with
  | Ujeqi n -> cmpq (imm32 n) (operand r1) ++ sete (operand_b r2)
  | Ujnei n -> cmpq (imm32 n) (operand r1) ++ setne (operand_b r2)
  | Ujlti n -> cmpq (imm32 n) (operand r1) ++ setl (operand_b r2)
  | Ujlei n -> cmpq (imm32 n) (operand r1) ++ setle (operand_b r2)
  | Ujgti n -> cmpq (imm32 n) (operand r1) ++ setg (operand_b r2)
  | Ujgei n -> cmpq (imm32 n) (operand r1) ++ setge (operand_b r2)
  | Ujz     -> test r1 ++ sete (operand_b r2)
  | Ujnz    -> test r1 ++ setne (operand_b r2))
  ++ andq (imm 0xff) (operand r2)

let bset bb r1 r2 r3 =
  cmpq (operand r2) (operand r1) ++
  (match bb with
  | Bjeq -> sete (operand_b r3)
  | Bjne -> setne (operand_b r3)
  | Bjlt -> setl (operand_b r3)
  | Bjle -> setle (operand_b r3)
  | Bjgt -> setg (operand_b r3)
  | Bjge -> setge (operand_b r3))
  ++ andq (imm 0xff) (operand r3)

										   
let rec lin g l =
  if Hashtbl.mem visited l then begin
	need_label l;
	emit (Label.fresh ()) (jmp (Label.to_string l))
  end else begin
	Hashtbl.add visited l ();
	instr g l (LMap.find l g)
  end

and instr g l = function
  | Ltl_ast.Eint (0l, r, l1) ->
	 emit l (zero r); lin g l1
  | Ltl_ast.Eint (n, r, l1) ->
	 emit l (movq (imm32 n) (operand r)); lin g l1
  | Ltl_ast.Eint64 (n, r, l1) ->
	 emit l (movabsq n (reg_to_q r)); lin g l1
  | Ltl_ast.Estring (s, r, l1) ->
	 emit l (movq (ilab (add_string s)) (operand r)); lin g l1
  | Ltl_ast.Egetfield (r1, n, r2, l1) ->
	 emit l (movq (ind ~ofs:(word_size * (n + 1)) (reg_to_q r1))
				  (reg (reg_to_q r2))); lin g l1
  | Ltl_ast.Esetfield (r1, n, r2, l1) ->
	 emit l (movq (reg (reg_to_q r2))
				  (ind ~ofs:(word_size * (n + 1)) (reg_to_q r1))
			); lin g l1
  | Ltl_ast.Ecall (s, l1) ->
	 emit l (call s); lin g l1
  | Ltl_ast.Ecallmethod (i, l1) ->
	 (* L'objet est dans %rdi,
        On utilise un register caller_saved qui n'est pas un paramètre
        pour l'appel, ici r10 *)
	 emit l ((movq (ind rdi) (reg r10)) ++
			 (*(call_star (ind ~ofs:(word_size * (i + 1)) r10)) *)
			 (movq (ind ~ofs:(word_size * (i + 1)) r10) (reg r10)) ++
			 (call_star (reg r10))
			);
	 lin g l1
  | Ltl_ast.Esetheader (s, r, l1) ->
	 emit l (movq (ilab (descr_label s)) (ind (reg_to_q r)));
	 lin g l1
  | Ltl_ast.Eunary (op, r, l1) ->
	 let t = match op with
	   | Xneg -> negq (operand r)
	   | Xnot -> xorq (imm32 1l) (operand r)
	   | Xaddi 0l -> nop
	   | Xaddi 1l -> incq (operand r)
	   | Xaddi (-1l) -> decq (operand r)
	   | Xaddi n -> addq (imm32 n) (operand r)
	   | Xmuli 0l -> zero r
	   | Xmuli 1l -> nop
	   | Xmuli (-1l) -> negq (operand r)
	   | Xmuli n -> imulq (imm32 n) (operand r)
	   | Xdivi n -> assert (r = Coloring.Reg Register.Rax);
					idivq (imm32 n)
	   | Xsli n -> shlq (imm n) (operand r)
	   | Xasri n -> sarq (imm n) (operand r)
	   | Xlsri n -> shrq (imm n) (operand r)
	 in
	 emit l t; lin g l1
  | Ltl_ast.Ebinary (op, r1, r2, l1) ->
	 let t = match op with
	   | Xadd -> addq (operand r1) (operand r2)
	   | Xsub -> subq (operand r1) (operand r2)
	   | Xmul -> imulq (operand r1) (operand r2)
	   | Xdiv -> assert (r2 = Coloring.Reg Register.Rax);
				 idivq (operand r1)
	   | Xsl  -> assert (r1 = Coloring.Reg Register.Rcx);
				 shlq (reg cl) (operand r2)
	   | Xasr -> assert (r1 = Coloring.Reg Register.Rcx);
				 sarq (reg cl) (operand r2);
	   | Xlsr -> assert (r1 = Coloring.Reg Register.Rcx);
				 shrq (reg cl) (operand r2)
	   | Xmov -> movq (operand r1) (operand r2)
	 in
	 emit l t; lin g l1
  | Ltl_ast.Ecqto l1 ->
	 emit l cqto; lin g l1
  | Ltl_ast.Egoto l1 ->
	 emit l nop; lin g l1
  | Ltl_ast.Eubranch (ub, r, l1, l2) when not (Hashtbl.mem visited l2) ->
	 need_label l1;
	 emit l (ubranch ub r (Label.to_string l1));
	 lin g l2;
	 lin g l1
  | Ltl_ast.Eubranch (ub, r, l1, l2) when not (Hashtbl.mem visited l1) ->
	 need_label l2;
	 emit l (ubranch (inv_ubranch ub) r (Label.to_string l2));
	 lin g l1;
	 lin g l2
  | Ltl_ast.Eubranch (ub, r, l1, l2) ->
	 need_label l1;
	 need_label l2;
	 emit l (ubranch ub r (Label.to_string l1) ++
			 jmp (Label.to_string l2))
  | Ltl_ast.Ebbranch (bb, r1, r2, l1, l2) when not (Hashtbl.mem visited l2) ->
	 need_label l1;
	 emit l (bbranch bb r1 r2 (Label.to_string l1));
	 lin g l2;
	 lin g l1
  | Ltl_ast.Ebbranch (bb, r1, r2, l1, l2) when not (Hashtbl.mem visited l1) ->
	 need_label l2;
	 emit l (bbranch (inv_bbranch bb) r1 r2 (Label.to_string l2));
	 lin g l1;
	 lin g l2
  | Ltl_ast.Ebbranch (bb, r1, r2, l1, l2) ->
	 need_label l1;
	 need_label l2;
	 emit l (bbranch bb r1 r2 (Label.to_string l1) ++
			 jmp (Label.to_string l2))
  | Ltl_ast.Euset (ub, r1, r2, l1) ->
	 emit l (uset ub r1 r2); lin g l1
  | Ltl_ast.Ebset (bb, r1, r2, r3, l1) ->
	 emit l (bset bb r1 r2 r3); lin g l1
  | Ltl_ast.Epush_param (r, l1) ->
	 emit l (pushq (operand r)); lin g l1
  | Ltl_ast.Ereturn ->
	 emit l ret

let func f =
  lin f.Ltl_ast.fun_body f.Ltl_ast.fun_entry;
  if f.Ltl_ast.fun_name = "main" then
	glabel "main" ++ get_code ()
  else
	label f.Ltl_ast.fun_name ++ get_code ()

let program p =
  let text = List.fold_left (++) nop
			   (List.map func p.Ltl_ast.prog_functions) in
  let data = get_strings () ++ class_descrs p.Ltl_ast.prog_class_descrs in
  { text = text;
	data = data }
