
let usage = "usage: pscala [options] file.scala"

let undefined_null_deref = ref false
let parse_only = ref false
let type_only = ref false
let rec spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
	"--type-only", Arg.Set type_only, "  stop after typing";
	"-G", Arg.Set Debug.enable_debug, "  show debug messages";
	"--undefined-null-deref", Arg.Set undefined_null_deref,
	  "  make null dereferencing cause an undefined behaviour \
	     (enables some optimisations)";
	"-h", Arg.Unit (fun () -> Arg.usage spec usage; exit 0),
	  "  Display this list of options";
  ]
