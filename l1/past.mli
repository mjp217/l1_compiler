(* 
   The Parsed AST 
*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint 
   | TEbool 
   | TEunit 
   | TEref of type_expr 
   | TEarrow of type_expr * type_expr
   | TEproduct of type_expr * type_expr
   | TEunion of type_expr * type_expr

type oper = ADD | MUL | DIV | SUB | GEQ

type unary_oper = NEG 

type expr = 
       | Integer of loc * int
       | Boolean of loc * bool
       | Location of loc * string
       | If of loc * expr * expr * expr
       | While of loc * expr * expr
       | UnaryOp of loc * unary_oper * expr
       | Op of loc * expr * oper * expr
	   | Seq of loc * (expr list)
      | Skip of loc
      | Deref of loc * expr
      | Assign of loc * expr * expr
      | Let of loc * var * expr * expr

val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_type : type_expr -> string 
val string_of_expr : expr -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit


