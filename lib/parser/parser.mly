%{
  open Lambdo_ast.Ast_type
%}

%token<string> IDENTIFIER
%token LAMBDA
%token ARROW
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token EOF

%start<expr> program
%nonassoc LEFT_PARENTHESIS 
%nonassoc LAMBDA
%nonassoc IDENTIFIER 
%left APP
%nonassoc FUN

%%
let program := ~ = expr; EOF; <>

let expr :=
  | e1 = expr; e2 = expr; %prec APP    { Call(e1, e2) }
  | s = IDENTIFIER;                  { Var s }
  | LAMBDA; s = IDENTIFIER; ARROW; e = expr; %prec FUN  { Lambda (s, e) }
  | LEFT_PARENTHESIS; ~ = expr; RIGHT_PARENTHESIS; <>