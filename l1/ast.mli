
type var = string 

type oper = ADD | MUL | DIV | SUB | GEQ 

type unary_oper = NEG 

type expr = 
       | Integer of int
       | Boolean of bool
       | Location of var
       | If of expr * expr * expr
       | While of expr * expr
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Seq of (expr list)
       | Skip
       | Deref of expr 
       | Assign of expr * expr
       | Let of var * expr * expr

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_uop : unary_oper -> string 
val string_of_bop : oper -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit
val string_of_expr : expr -> string 
