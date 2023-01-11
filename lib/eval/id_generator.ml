module type Generator_type = sig
  val new_id : unit -> Lambdo_ast.Ast_type.identifier
end

module Generator : Generator_type = struct
  let count = ref 0

  let new_id () =
    let id = string_of_int !count ^ "v" in
    count := succ !count;
    id
end