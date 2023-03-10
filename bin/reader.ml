open Lambdo_parser
open Lambdo_ast.Ast_type
open Lambdo_eval.Eval

let eval_string input =
  let buffer = Lexing.from_string input in
  try
    let ast = Parser.program Lexer.read buffer in
    let evaluated_ast = eval ast in
    print_string (pp evaluated_ast)
  with
  | Lexer.LexingError msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start buffer)

let rec repl () =
  print_string "\nλ> ";
  let input = read_line () in
  eval_string input;
  repl ()
