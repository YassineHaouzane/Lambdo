%{
  open Ast_type
%}

%token<string> IDENTIFIER
%token LAMBDA
%token ARROW
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token EOF

%start<Ast_type.expr> program
%nonassoc LEFT_PARENTHESIS 
%nonassoc ARROW LAMBDA
%nonassoc IDENTIFIER 
%left APP

%%
let program := ~ = expr; EOF; <>

let expr :=
  | e1 = expr; e2 = expr; %prec APP    { Call(e1, e2) }
  | s = IDENTIFIER;                  { Var s }
  | LAMBDA; s = IDENTIFIER; ARROW; e = expr;  { Lambda (s, e) }
  | LEFT_PARENTHESIS; ~ = expr; RIGHT_PARENTHESIS; <>