type identifier = string [@@deriving show]

type expr =
  | Var of identifier
  | Lambda of identifier * expr
  | Call of expr * expr
[@@deriving show]