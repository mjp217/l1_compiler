(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB
  | Past.GEQ -> Ast.GEQ


let rec translate_expr = function 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Boolean(_, v)     -> Ast.Boolean v
    | Past.If(_, e1, e2, e3)    -> Ast.If(translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.UnaryOp(_, op, e) -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.Seq(_, e1) -> Ast.Seq(List.map translate_expr e1)
