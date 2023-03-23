open Lambdo_ast.Ast_type
open Id_generator
module Identifier_set = Set.Make (String)

let free_variables expr =
  let rec compute_FV expr free_vars bound_vars =
    match expr with
    | Var x when Identifier_set.mem x bound_vars -> free_vars
    | Var x -> Identifier_set.add x free_vars
    | Lambda (x, e1) ->
        compute_FV e1 free_vars (Identifier_set.add x bound_vars)
    | Call (e1, e2) ->
        let fv_e1 = compute_FV e1 free_vars bound_vars in
        let fv_e2 = compute_FV e2 free_vars bound_vars in
        Identifier_set.union fv_e1 fv_e2
  in
  compute_FV expr Identifier_set.empty Identifier_set.empty

let rec alpha_conversion expr old_id new_id =
  match expr with
  | Var x when x = old_id -> Var new_id
  | Var _ -> expr
  | Call (e1, e2) ->
      let alpha_converted_e1 = alpha_conversion e1 old_id new_id in
      let alpha_converted_e2 = alpha_conversion e2 old_id new_id in
      Call (alpha_converted_e1, alpha_converted_e2)
  | Lambda (x, _) when x = old_id -> expr
  | Lambda (x, e1) ->
      let alpha_converted_e1 = alpha_conversion e1 old_id new_id in
      Lambda (x, alpha_converted_e1)

let rec replace_all_recursively identifier fun_body replacer =
  match fun_body with
  | Var x when x = identifier -> replacer
  | Var _ -> fun_body
  | Lambda (x, _) when x = identifier -> fun_body
  | Lambda (x, e1) when not (Identifier_set.mem x (free_variables replacer)) ->
      let e1_replaced = replace_all_recursively identifier e1 replacer in
      Lambda (x, e1_replaced)
  | Lambda (x, e1) ->
      let new_id = Generator.new_id () in
      let alpha_converted_lambda = alpha_conversion e1 x new_id in
      let replaced_e1 =
        replace_all_recursively identifier alpha_converted_lambda replacer
      in
      Lambda (new_id, replaced_e1)
  | Call (e1, e2) ->
      let e1_replaced = replace_all_recursively identifier e1 replacer in
      let e2_replaced = replace_all_recursively identifier e2 replacer in
      Call (e1_replaced, e2_replaced)

let replace_all e1 e2 =
  match e1 with
  | Lambda (id, fun_body) -> replace_all_recursively id fun_body e2
  | _ ->
      let expr_string = pp e1 in
      let parameter_string = pp e2 in
      let error_msg =
        Format.sprintf "Error: trying to apply expr: %s as a function to %s"
          expr_string parameter_string
      in
      raise (Failure error_msg)

(* applies beta reduction until we are in normal form
   and applies alpha conversion if necessary *)
let rec eval expr =
  match expr with
  | Var _ -> expr
  | Call (e1, e2) ->
      let e1_eval = eval e1 in
      let e2_eval = eval e2 in
      replace_all e1_eval e2_eval
  (* Lazy evaluation *)
  | Lambda (_, _) -> expr

let%test "simple beta reduction" =
  eval (Call (Lambda ("x", Var "x"), Var "x")) = Var "x"
