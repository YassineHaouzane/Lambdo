type identifier = string

type expr =
  | Var of identifier
  | Lambda of identifier * expr
  | Call of expr * expr

let rec pp = function
  | Var x -> x
  | Lambda (id, e) -> "\\" ^ id ^ " -> " ^ pp e
  | Call (e1, e2) -> pp e1 ^ " " ^ pp e2
