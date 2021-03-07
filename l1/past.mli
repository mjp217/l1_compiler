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
       | Unit of loc  
       | What of loc 
       | Var of loc * var
       | Integer of loc * int
       | Boolean of loc * bool
       | Location of loc * string
       | UnaryOp of loc * unary_oper * expr
       | Op of loc * expr * oper * expr
       | If of loc * expr * expr * expr
       | Pair of loc * expr * expr
       | Fst of loc * expr 
       | Snd of loc * expr 
       | Inl of loc * type_expr * expr 
       | Inr of loc * type_expr * expr 
       | Case of loc * expr * lambda * lambda 
       | While of loc * expr * expr 
       | Seq of loc * (expr list)
       | Ref of loc * expr 
       | Skip of loc
       | Deref of loc * expr
       | Assign of loc * expr * expr
       | Let of loc * var * expr * expr


and lambda = var * type_expr * expr 

val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 

(* printing *) 
val string_of_unary_oper : unary_oper -> string 
val string_of_oper : oper -> string 
val string_of_type : type_expr -> string 
val string_of_expr : expr -> string 
val print_expr : expr -> unit 
val eprint_expr : expr -> unit


