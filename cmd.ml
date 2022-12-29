let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let parse_args () =
  match Array.length Sys.argv with
  | 2 ->
      let argv1 = Sys.argv.(1) in
      if argv1 = "--r" then Reader.repl ()
      else
        let filename = argv1 in
        let s = read_whole_file filename in
        Reader.eval_string s
  | _ -> print_string "Can't handle multiple argument "
