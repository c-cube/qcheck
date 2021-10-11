(**
   Adapted from https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem
   We want to avoid listing files manually (likely forgetting some),
   Instead this will output a stanza along these lines automatically:

(tests
  (names
    int_zero_qcheck
    int_zero_qcheck2
    ...
  )
  (modules
    log
    int_zero_qcheck
    int_zero_qcheck2
    ...
  )
  (libraries qcheck)
)

*)

let print_stanza filenames =
  Printf.printf
    {|
(tests
  (names
    %s
  )
  (modules
    log
    %s
  )
  (libraries qcheck)
)
|} filenames filenames

let is_shrink_test filename = match filename with
  | "log.ml"
  | "gen_dune_rule.ml" -> false
  | _ -> Filename.check_suffix filename ".ml"

let () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_shrink_test
  |> List.map Filename.remove_extension
  |> fun fs -> String.concat "\n    " fs
  |> print_stanza
