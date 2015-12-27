open Type_ast
open X86_64

let make_label = begin
	let label_index = ref 0 in
	(fun s ->
     incr label_index;
	 "L_" ^ s ^ "_" ^ (string_of_int !label_index))
  end

let method_label = begin
	let t = Hashtbl.create 17 in
	let existing = Hashtbl.create 17 in
	(fun class_name method_name ->
	 let try_create lname =
	   if Hashtbl.mem existing lname then
		 false
	   else begin
		 Hashtbl.add existing lname ();
		 Hashtbl.add t (class_name, method_name) lname;
		 true
		 end
	 in
	 try Hashtbl.find t (class_name, method_name) with
	   Not_found ->
	   let lname = ref ("M_" ^ class_name ^ "_" ^ method_name) in
	   let c = ref 1 in
	   while not (try_create !lname) do
		 lname := "M_" ^ class_name ^ "_" ^ method_name ^ "_" ^ (string_of_int !c);
		 incr c
	   done;
	   !lname)
  end

let class_label class_name = "C_" ^ class_name

let descr_label class_name = "D_" ^ class_name
									  
let produce_code prog = {
  text = glabel "main" ++
		 xorq (reg rax) (reg rax) ++
		 ret
  ;
  data = nop }
