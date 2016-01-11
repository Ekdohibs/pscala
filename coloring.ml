open Ertl_ast
open Common
open Interference

type color = Spilled of int | Reg of Register.reg
module Cset = Set.Make(struct type t = color let compare = compare end)
type coloring = color Rmap.t

let remove g v =
  Rmap.map (fun arc ->
    { prefs = Rset.remove v arc.prefs;
	  interf = Rset.remove v arc.interf })
		   (Rmap.remove v g)

let fusion g v1 v2 =
  let arc1 = Rmap.find v1 g in
  let others = Rmap.remove v1 g in
  let update s =
	if Rset.mem v1 s then Rset.add v2 (Rset.remove v1 s) else s
  in
  let merge s1 s2 =
	Rset.remove v1 (Rset.remove v2 (Rset.union s1 s2))
  in
  Rmap.mapi (fun v arc ->
	if v = v2 then
	  let prefs = merge arc.prefs arc1.prefs in
	  let interf = merge arc.interf arc1.interf in
	  { interf = interf; prefs = Rset.diff prefs interf }
	else
	  let prefs = update arc.prefs in
	  let interf = update arc.interf in
	  { interf = interf; prefs = Rset.diff prefs interf })
  others
  
let forget_pref g v1 =
  Rmap.mapi (fun v arc ->
	if v = v1 then { arc with prefs = Rset.empty }
	else { arc with prefs = Rset.remove v1 arc.prefs }) g

(* On réserve r10, r11 comme temporaires *)
let coloring_registers = Register.([Rdi; Rsi; Rdx; Rcx; R8; R9;
									Rbx; Rbp; R12; R13; R14; R15; Rax])
let coloring_set =
  List.fold_left (fun s x -> Cset.add (Reg x) s)
				 Cset.empty coloring_registers
let k = List.length coloring_registers

let find p s =
  Rset.choose (Rset.filter p s)

let george g interf v2 =
  let int2 = (Rmap.find v2 g).interf in
  Rset.for_all (fun v ->
	Rset.cardinal (Rmap.find v g).interf < k)
			   (Rset.diff interf int2)

let cout g v =
  -. (float_of_int (Rset.cardinal (Rmap.find v g).interf))
			   
let rec simplify g =
  let bests, bestd =
	Rmap.fold (fun s arc (bests, bestd) ->
	  if Rset.cardinal arc.interf < bestd && Rset.is_empty arc.prefs
	     && not (Register.is_real s) then
		s, Rset.cardinal arc.interf
	  else
		bests, bestd) g (Register.rax, max_int) in
  if bestd < k then
	select g bests
  else
	coalesce g

and coalesce g =
  let v1, v2, found = Rmap.fold (fun s1 arc ((_, _, found) as r) ->
	if found || Register.is_real s1 then r else
	try let s2 = find (george g arc.interf) arc.prefs in
		(s1, s2, true)
	with Not_found -> r) g (Register.rax, Register.rax, false) in
  if found then
	let c = simplify (fusion g v1 v2) in
	Rmap.add v1 (Rmap.find v2 c) c
  else
	freeze g

and freeze g =
  let bests, bestd =
	Rmap.fold (fun s arc (bests, bestd) ->
	  if Rset.cardinal arc.interf < bestd
	     && not (Register.is_real s) then
		s, Rset.cardinal arc.interf
	  else
		bests, bestd) g (Register.rax, max_int) in
  if bestd < k then
	simplify (forget_pref g bests)
  else
	spill g

and spill g =
  if Rmap.for_all (fun r _ -> Register.is_real r) g then
	Rmap.mapi (fun r _ ->
			   match r with
			   | Register.Reg r -> Reg r
			   | _ -> assert false) g
  else
	let (cost, s) = Rmap.fold (fun s arc (bestcost, bests) ->
	   if Register.is_real s then (bestcost, bests) else
		 let c = cout g s in
		 if c < bestcost then
		   (c, s)
		 else
		   (bestcost, bests)) g (max_float, Register.rax) in
	select g s

and select g v =
  let c = simplify (remove g v) in
  let impossible = Rset.fold (fun v imp ->
	  Cset.add (Rmap.find v c) imp) (Rmap.find v g).interf Cset.empty in
  let possible = Cset.diff coloring_set impossible in
  try
	Rmap.add v (Cset.choose possible) c
  with
	Not_found -> Rmap.add v (Spilled 0) c

let subgraph g s =
  Rmap.map (fun arc ->
    { prefs = Rset.inter s arc.prefs;
	  interf = Rset.inter s arc.interf })
  (Rmap.filter (fun r _ -> Rset.mem r s) g)
						  
let rec contract g =
  let v1, v2, found = Rmap.fold (fun s1 arc ((_, _, found) as r) ->
	if found then r else
	try let s2 = Rset.choose arc.prefs in
		(s1, s2, true)
	with Not_found -> r) g (Register.rax, Register.rax, false) in
  if found then
	let c = contract (fusion g v1 v2) in
	Rmap.add v1 (Rmap.find v2 c) c
  else
	simpl g

and simpl g =
  let bests, bestd =
	Rmap.fold (fun s arc (bests, bestd) ->
	  if Rset.cardinal arc.interf < bestd && Rset.is_empty arc.prefs
	     && not (Register.is_real s) then
		s, Rset.cardinal arc.interf
	  else
		bests, bestd) g (Register.rax, max_int) in
  if bestd < max_int then
	select g bests
  else
	Rmap.empty
		   
and select g v =
  let c = simpl (remove g v) in
  let impossible = Rset.fold (fun v imp ->
	  Cset.add (Rmap.find v c) imp) (Rmap.find v g).interf Cset.empty in
  Rmap.add v (let rec try_find i =
				if Cset.mem (Spilled i) impossible then
				  try_find (i + 1)
				else Spilled i
			  in try_find 0) c
						  
let find_coloring g =
  let c = simplify g in
  let u = Rmap.fold (fun r color s ->
		   match color with
		   | Spilled _ -> Rset.add r s
		   | _ -> s) c Rset.empty in
  let sg = subgraph g u in
  let c2 = contract sg in
  let c = Rmap.mapi (fun r color ->
			 match color with
			 | Spilled _ -> Rmap.find r c2
			 | _ -> color) c in
  let nlocals = Rmap.fold (fun r color m ->
			 match color with
			 | Spilled n -> max m (n + 1)
			 | _ -> m) c 0 in
  c, nlocals

			
