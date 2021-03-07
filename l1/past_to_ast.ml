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
    | Past.Unit _            -> Ast.Unit
    | Past.What _            -> Ast.UnaryOp(Ast.READ, Ast.Unit)
    | Past.Var(_, x)         -> Ast.Var x 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Boolean(_, b)     -> Ast.Boolean b
    | Past.UnaryOp(_, op, e) -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.If(_, e1, e2, e3) -> Ast.If(translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.Pair(_, e1, e2)   -> Ast.Pair(translate_expr e1, translate_expr e2)
    | Past.Fst(_, e)         -> Ast.Fst(translate_expr e)
    | Past.Snd(_, e)         -> Ast.Snd(translate_expr e)
    | Past.Inl(_, _, e)       -> Ast.Inl(translate_expr e)
    | Past.Inr(_, _, e)       -> Ast.Inr(translate_expr e)
    | Past.Seq(_, el) -> Ast.Seq(List.map translate_expr el)
    | Past.Ref(_, e) -> Ast.Ref(translate_expr e)
    | Past.Deref(_, e) -> Ast.Deref(translate_expr e)
    | Past.Assign(_, e1, e2) -> Ast.Assign(translate_expr e1, translate_expr e2)

and translate_lambda (x, _, body) = (x, translate_expr body) 