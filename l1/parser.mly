/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT 
%token <string> LOC
%token ADD SUB MUL DIV SEMICOLON
%token IF THEN ELSE 
%token WHILE DO 
%token LET IN
%token ASSIGN DEREF 
%token TRUE FALSE
%token LPAREN RPAREN
%token BEGIN END
%token GEQ
%token SKIP
%token EOF
%left ADD SUB        /* lowest precedence */
%left MUL DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| TRUE                               { Past.Boolean (get_loc(), true) }
| FALSE                              { Past.Boolean (get_loc(), false) }
| LOC                                { Past.Location (get_loc(), $1) }
| SKIP                               { Past.Skip (get_loc()) }
| DEREF simple_expr                  { Past.Deref (get_loc(), $2) }
| LPAREN expr RPAREN                 { $2 }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr GEQ expr                      { Past.Op(get_loc(), $1, Past.GEQ, $3) }
| LET LOC ASSIGN expr IN expr END    { Past.Let(get_loc(), $2, $4, $6) }
| simple_expr ASSIGN expr            		 { Past.Assign(get_loc(), $1, $3) }
| IF expr THEN expr ELSE expr        { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr                 { Past.While(get_loc(), $2, $4) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


