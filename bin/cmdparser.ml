open Cmdliner

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let parse_args repl filepath_o =
  match (repl, filepath_o) with
  | true, Some _ ->
      Printf.eprintf "Error: REPL mode and SRC_FILE are mutually exclusive"
  | true, _ -> Reader.repl ()
  | false, Some filepath ->
      let s = read_whole_file filepath in
      Reader.eval_string s
  | false, None -> Reader.repl ()

let repl_term =
  let doc = "Toggles REPL mode." in
  Arg.(value & flag & info [ "r"; "repl" ] ~doc)

let filepath_term =
  let doc = "Path of the file to run." in
  let docv = "SRC_FILE" in
  Arg.(value & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)

let lambdo_t = Term.(const parse_args $ repl_term $ filepath_term)

let cmd =
  let doc = "print a customizable message repeatedly" in
  let info = Cmd.info "Lambdo" ~doc in
  Cmd.v info lambdo_t
