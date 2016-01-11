open Ertl_ast
open Common

type arcs = {
  prefs : Rset.t;
  interf : Rset.t
}
type graph = arcs Rmap.t
				  
let make infos =
  let graph = Hashtbl.create 17 in
  let get r = try Hashtbl.find graph r with
				Not_found -> { prefs = Rset.empty; interf = Rset.empty } in
  let add_interf1 v w =
	let u = get v in
	Hashtbl.replace graph v { u with interf = Rset.add w u.interf }
  in
  let add_pref1 v w =
	let u = get v in
	Hashtbl.replace graph v { u with prefs = Rset.add w u.prefs }
  in
  let add_interf v w = if v <> w then (add_interf1 v w; add_interf1 w v) in
  let add_pref v w = if v <> w then (add_pref1 v w; add_pref1 w v) in
  LMap.iter (fun _ (instr, def, _, live_out) ->
			 match instr with
			 | Ebinary (Xmov, r1, r2, _) ->
				Rset.iter (fun r -> if r <> r2 then add_interf r r1) live_out;
				add_pref r1 r2
			 | _ ->
				Rset.iter (fun r1 ->
                  Rset.iter (add_interf r1) live_out) def
			) infos;
  Hashtbl.fold
   (fun r arc mp ->
     Rmap.add r { arc with prefs = Rset.diff arc.prefs arc.interf } mp)
   graph Rmap.empty
