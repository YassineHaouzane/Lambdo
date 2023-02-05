open Cmdliner

let () = exit (Cmd.eval Cmdparser.cmd)
