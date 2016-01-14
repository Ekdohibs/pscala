open Ertl_ast
open Common

let rec take n l =
  if n = 0 then [] else
	match l with
	  [] -> []
	| x :: t -> x :: (take (n - 1) t)
	   
let def_use = function
  | Eint (_, r, _) -> [r], []
  | Estring (_, r, _) -> [r], []
  | Eunit (r, _) -> [r], []
  | Egetfield (r1, _, r2, _) -> [r2], [r1]
  | Esetfield (r1, _, r2, _) -> [], [r1; r2]
  | Ecall (_, n, _) | Ecallmethod (_, n, _) ->
	 Register.caller_saved, take n Register.parameters
  | Esetheader (_, r, _) -> [], [r]
  | Eunary (Xdivi _, r, _) ->
	 assert (r = Register.rax);
	 [Register.rax; Register.rdx], [Register.rax]
  | Eunary (_, r, _) -> [r], [r]
  | Ebinary (Xdiv, r1, r2, _) ->
	 assert (r2 = Register.rax);
	 [Register.rax; Register.rdx], [r1; Register.rax; Register.rdx]
  | Ecqto _ ->
	 [Register.rdx], [Register.rax]
  | Ebinary (Xmov, r1, r2, _) -> [r2], [r1]
  | Ebinary3 (_, r1, r2, r3, _) -> [r3], [r1; r2]
  | Ebinary (_, r1, r2, _) -> [r2], [r1; r2]
  | Egoto _ | Ealloc_frame _ | Edelete_frame _ -> [], []
  | Eubranch (_, r, _, _) -> [], [r]
  | Ebbranch (_, r1, r2, _, _) -> [], [r1; r2]
  | Euset (_, r1, r2, _) -> [r2], [r1]
  | Ebset (_, r1, r2, r3, _) -> [r3], [r1; r2]
  | Eget_param (_, r, _) -> [r], []
  | Epush_param (r, _) -> [], [r]
  | Estack_free _ -> [], []
  | Ereturn -> [], Register.rax :: Register.callee_saved

type liveliness_info = {
  live_def : Rset.t;
  live_use : Rset.t;
  live_next : label list;
  mutable live_pred : label list
}
									 
let analyse graph =
  let infos = Hashtbl.create 17 in
  LMap.iter (fun l instr ->
	let def, use = def_use instr in
	Hashtbl.add infos l
	 {
	   live_def = List.fold_right Rset.add def Rset.empty;
	   live_use = List.fold_right Rset.add use Rset.empty;
	   live_next = Ertl.next_labels instr;
	   live_pred = []
	 })
    graph;
  Hashtbl.iter (fun l info ->
	List.iter (fun n ->
	 let u = Hashtbl.find infos n in
	 u.live_pred <- l :: u.live_pred
    ) info.live_next) infos;
  let live_in = Hashtbl.create 17 in
  let live_out = Hashtbl.create 17 in
  let sommets = ref [] in
  LMap.iter (fun l _ ->
			 Hashtbl.add live_in l Rset.empty;
			 Hashtbl.add live_out l Rset.empty;
			 sommets := l :: !sommets) graph;
  while !sommets != [] do
	let l, t = List.hd !sommets, List.tl !sommets in
	sommets := t;
	let info = Hashtbl.find infos l in
	let old_in = Hashtbl.find live_in l in
	let out = List.fold_left (fun s ll ->
				Rset.union s (Hashtbl.find live_in ll))
							 Rset.empty info.live_next in
	let new_in = Rset.union info.live_use (Rset.diff out info.live_def) in
	Hashtbl.replace live_in l new_in;
	Hashtbl.replace live_out l out;
	if not (Rset.equal new_in old_in) then
	  sommets := info.live_pred @ !sommets
  done;
  LMap.mapi (fun l instr ->
	instr, (Hashtbl.find infos l).live_def,
	Hashtbl.find live_in l, Hashtbl.find live_out l)
  graph


let rec print_graph ff graph infos seen i =
  if Hashtbl.mem seen i then () else begin
	Hashtbl.add seen i ();
	try
	  let instr = LMap.find i graph in
	  let print_rset ff s = print_list ff Register.print
									   ", " (Rset.elements s) in
	  let _, _, live_in, live_out = LMap.find i infos in
	  Format.fprintf ff "%a: %a\tin = %a\tout = %a@\n"
					 Label.print i
					 Ertl.print_instr instr
					 print_rset live_in
					 print_rset live_out
	  ;
	  List.iter (print_graph ff graph infos seen)
				(Ertl.next_labels instr)
	with
	  Not_found -> ()
  end

let print_func ff f =
  let infos = analyse f.fun_body in
  Format.fprintf ff "%s(%d)@\n" f.fun_name f.fun_params;
  Format.fprintf ff "Entry: %a@[<v 2>@\n" Label.print f.fun_entry;
  print_graph ff f.fun_body infos
			  (Hashtbl.create 17) f.fun_entry;
  Format.fprintf ff "@]@."

let print_program ff p =
  print_list ff print_func "@\n" p.prog_functions;
  Format.fprintf ff "Class descriptors:@\n";
  print_list ff (fun ff (class_name, parent_name, data) ->
				 Format.fprintf ff "%s (inherits %s)@[<v 2>@\n"
								class_name parent_name;
				 print_list ff (fun ff -> Format.fprintf ff "%s") "@\n" data;
				 Format.fprintf ff "@]")
			 "@\n@\n" p.prog_class_descrs;
  Format.fprintf ff "@."
