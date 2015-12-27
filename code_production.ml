open Ast
open Type_ast
open X86_64

let make_label = begin
	let label_index = ref 0 in
	(fun s ->
     incr label_index;
	 Format.sprintf "L_%s_%d" s !label_index)
  end

let make_data_label = begin
	let label_index = ref 0 in
	(fun s ->
     incr label_index;
	 Format.sprintf "R_%s_%d" s !label_index)
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
	   let lname = ref (Format.sprintf "M_%s_%s" class_name method_name) in
	   let c = ref 1 in
	   while not (try_create !lname) do
		 lname := Format.sprintf "M_%s_%s_%d" class_name method_name !c;
		 incr c
	   done;
	   !lname)
  end

let constr_label class_name = "C_" ^ class_name

let descr_label class_name = "D_" ^ class_name

type class_repr = {
  r_num_fields : int;
  r_num_methods : int;
  r_cp_offset : int;
  r_methods : (int * string) Smap.t;
  r_lmethods : p_ident list;
  r_vars : int Smap.t;
  r_parent : p_ident
}

let make_base_repr parent = {
  r_num_fields = 0; r_num_methods = 0;
  r_methods = Smap.empty; r_vars = Smap.empty;
  r_lmethods = []; r_parent = parent;
  r_cp_offset = 0
}
let br = [
  "Any", make_base_repr "Any";
  "AnyRef", make_base_repr "AnyRef"
]
let base_reprs =
  List.fold_left (fun m (n, v) -> Smap.add n v m) Smap.empty br
					
let add_repr c_name cls c_reprs =
  let parent_name = match (fst cls.c_extends).t_type_name with
	  TGlobal s -> s | _ -> assert false in
  let parent_repr = Smap.find parent_name c_reprs in
  let (nf, f) = List.fold_left (fun (nf, f) (_, _, name) ->
	  (nf + 1, Smap.add name nf f))
	  (parent_repr.r_num_fields + List.length cls.c_params,
	   parent_repr.r_vars) cls.c_vars in
  let (nm, lm, m) = Smap.fold (fun name _ (nm, lm, m) ->
	  try
		let (i, _) = Smap.find name m in
		(nm, lm, Smap.add name (i, c_name) m)
	  with Not_found ->
		(nm + 1, name :: lm, Smap.add name (nm, c_name) m))
	 cls.c_methods
	 (parent_repr.r_num_methods, [], parent_repr.r_methods) in
  let repr = {
	r_num_fields = nf; r_vars = f;
	r_num_methods = nm; r_methods = m;
	r_lmethods = parent_repr.r_lmethods @ (List.rev lm);
	r_cp_offset = parent_repr.r_num_fields;
	r_parent = parent_name
  } in
  Smap.add c_name repr c_reprs

let compute_reprs l_classes =
  List.fold_left (fun r (name, cls) -> add_repr name cls r)
	base_reprs l_classes

let reprs_data reprs =
  Smap.fold (fun name repr e ->
   e ++
	 label (descr_label name) ++
	 address [descr_label repr.r_parent] ++
	 address (List.map
	   (fun mn -> method_label (snd (Smap.find mn repr.r_methods)) mn)
	   repr.r_lmethods))
	reprs nop
	
let produce_code prog =
  let reprs = compute_reprs prog in
  let r_data = reprs_data reprs in
  {
	text = glabel "main" ++
		 xorq (reg rax) (reg rax) ++
		 ret
    ;
    data = r_data
  }
