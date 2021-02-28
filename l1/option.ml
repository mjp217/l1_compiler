(*
   parse command line options and args, using the basic structure from the Slang interpreter
*)
let infile         = ref ""
let verbose        = ref false
let verbose_front  = ref false
let verbose_tree  = ref false
let run_tests      = ref false
let use_i0         = ref false		 
let use_all ()     =
   use_i0 := true
let show_compiled  = ref false
let set_infile f   = infile := f
let stack_max      = ref 1000
let heap_max       = ref 1000

let option_spec = [
     ("-V",    Arg.Set verbose_front, "verbose front end");
     ("-v",    Arg.Set verbose,       "verbose interpreter(s)");
     ("-T",    Arg.Set verbose_tree,       "verbose output in the form of tree (currently only frontend)");
     ("-c",    Arg.Set show_compiled, "show compiled code (but don't run it)");
     ("-i0",   Arg.Set use_i0,        "Interpreter 0");  
     ("-all",  Arg.Unit use_all,      "all interpreters");
     ("-stackmax",  Arg.Set_int stack_max, "set max stack size (default = 1000)");
     ("-heapmax",  Arg.Set_int heap_max, "set max heap size (default = 1000)");
     ("-t",    Arg.Set run_tests,     "run all test/*.l1 with each selected interpreter, report unexpected outputs (silent otherwise)")
    ]
let usage_msg = "Usage: l1.byte [options] [<file>]\nOptions are:"

(* This does the parsing and *)
let () = Arg.parse option_spec set_infile usage_msg

(* set immutable versions of the options now that they have been parsed 
 * Note: this is only to make the interface cleaner. *)
let infile        = !infile
let verbose       = !verbose
let verbose_front = !verbose_front
let verbose_tree = !verbose_tree
let run_tests     = !run_tests
let use_i0        = !use_i0	     
let show_compiled = !show_compiled
let stack_max     = !stack_max
let heap_max      = !heap_max
