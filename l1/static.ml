(* using the structure from the Slang interpreter *)

open Past 

let complain = Errors.complain

let internal_error msg = complain ("INTERNAL ERROR: " ^ msg) 

let report_expecting e msg t = 
    let loc = loc_of_expr e in 
    let loc_str = string_of_loc loc in 
    let e_str = string_of_expr e in 
    let t_str = string_of_type t in 
    complain ("ERROR at location " ^ 
	      loc_str ^ "\nExpression " ^ e_str ^ 
	      "\nhas type " ^ t_str ^ ", but expecting " ^ msg) 

let report_types_not_equal loc t1 t2 = 
    let loc_str = string_of_loc loc in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("Error near location " ^ loc_str ^ 
              "\nExpecting type " ^ t1_str ^ " to be equal to type " ^ t2_str)

let report_type_mismatch (e1, t1) (e2, t2) = 
    let loc1 = loc_of_expr e1 in 
    let loc2 = loc_of_expr e2 in 
    let loc1_str = string_of_loc loc1 in 
    let loc2_str = string_of_loc loc2 in 
    let e1_str = string_of_expr e1 in 
    let e2_str = string_of_expr e2 in 
    let t1_str = string_of_type t1 in 
    let t2_str = string_of_type t2 in 
    complain ("ERROR, Type Mismatch: expecting equal types, however\n" ^ 
	      "at location " ^ loc1_str ^ "\nexpression " ^ e1_str ^ "\nhas type " ^ t1_str ^ 
	      " and at location " ^ loc2_str ^ "\nexpression " ^ e2_str ^ "\nhas type " ^ t2_str)

let rec find loc x = function 
  | [] -> complain (x ^ " is not defined at " ^ (string_of_loc loc)) 
  | (y, v) :: rest -> if x = y then v else find loc x rest


(* may want to make this more interesting someday ... *) 
let rec match_types (t1, t2) = (t1 = t2) 

let make_uop loc uop (e, t) = 
    match uop, t with 
    | NEG, TEint  -> (UnaryOp(loc, uop, e), t) 
    | NEG, t'     -> report_expecting e "integer" t

let make_bop loc bop (e1, t1) (e2, t2) = 
    match bop, t1, t2 with 
    | ADD, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | ADD, TEint,  t      -> report_expecting e2 "integer" t
    | ADD, t,      _      -> report_expecting e1 "integer" t
    | SUB, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | SUB, TEint,  t      -> report_expecting e2 "integer" t
    | SUB, t,      _      -> report_expecting e1 "integer" t
    | MUL, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | MUL, TEint,  t      -> report_expecting e2 "integer" t
    | MUL, t,      _      -> report_expecting e1 "integer" t
    | DIV, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | DIV, TEint,  t      -> report_expecting e2 "integer" t
    | DIV, t,      _      -> report_expecting e1 "integer" t
    | GEQ, TEint, TEint   -> (Op(loc, e1, bop, e2), TEbool)
    | GEQ, TEint, t       -> report_expecting e2 "integer" t
    | GEQ, t,     _       -> report_expecting e2 "integer" t

let make_if loc (e1, t1) (e2, t2) (e3, t3) =
     match t1 with
     | TEbool ->
          if match_types (t2, t3)
          then (If(loc, e1, e2, e3), t2)
          else report_type_mismatch (e2, t2) (e3, t3)
      | ty -> report_expecting e1 "boolean" ty

let make_while loc (e1, t1) (e2, t2) =
     match t1 with
     | TEbool -> (While(loc, e1, e2), TEunit)
      | ty -> report_expecting e1 "boolean" ty

let rec  infer env e = 
    match e with 
    | Integer _            -> (e, TEint)
    | Boolean _            -> (e, TEbool)
    | Location (loc, e1)   -> (e, find loc e1 env)
    | If (loc, e1, e2, e3)      -> make_if loc (infer env e1) (infer env e2) (infer env e3)  
    | While (loc, e1, e2)  -> make_while loc (infer env e1) (infer env e2)
    | UnaryOp(loc, uop, e) -> make_uop loc uop (infer env e) 
    | Op(loc, e1, bop, e2) -> make_bop loc bop (infer env e1) (infer env e2) 
    | Seq(loc, el)         -> infer_seq loc env el 
    | Skip(loc)            -> (e, TEunit)
    | Deref(loc, e1)       -> (Deref(loc, e), TEint)
    | Assign(loc, e1, e2)  -> (Assign(loc, e1, e2), TEunit)
    | Let(loc, e1, e2, e3) -> let (_, t) = (infer env e3) in (Let(loc, e1, e2, e3), t)

and infer_seq loc env el = 
    let rec aux carry = function 
      | []        -> internal_error "empty sequence found in parsed AST" 
      | [e]       -> let (e', t) = infer env e in (Seq(loc, List.rev (e' :: carry )), t)
      | e :: rest -> let (e', _) = infer env e in aux (e' :: carry) rest 
    in aux [] el 

let env_init = [] 

let check e = 
    let (e', _) = infer env_init e 
    in e' 

