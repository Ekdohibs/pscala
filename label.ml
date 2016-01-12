
type t = int
		   
let fresh = begin
	let index = ref 0 in
	(fun () ->
	 incr index;
	 !index)
  end

let compare = compare

let print ff l =
  Format.fprintf ff "L%d" l

let to_string l = "L" ^ (string_of_int l)
