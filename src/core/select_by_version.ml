(* Not part of qcheck-core: dune runs it with the toplevel at build time.

     ocaml -noinit select_by_version.ml MAJOR.MINOR FROM BEFORE [VERSION]

   prints the contents of FROM if the compiler version (or VERSION) is at
   least MAJOR.MINOR, and those of BEFORE otherwise. Versions are compared
   numerically, unlike in enabled_if. [--self-test] checks the comparison
   on a list of versions. *)

let parse version = Scanf.sscanf version "%d.%d" (fun major minor -> (major, minor))

let at_least threshold version = compare (parse version) (parse threshold) >= 0

let cat file =
  let ic = open_in_bin file in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  print_string contents

let self_test () =
  let cases = [
    "5.6", "5.6.0", true;
    "5.6", "5.6.0+dev2-2026-10-02", true;
    "5.6", "5.10.0", true;
    "5.6", "5.11.1+flambda", true;
    "5.6", "6.0.0", true;
    "5.6", "5.5.0", false;
    "5.6", "5.0.0~beta1", false;
    "5.6", "4.14.2", false;
    "5.6", "4.08.1", false;
    "5.0", "4.14.2", false;
    "5.0", "5.0.0", true;
  ] in
  let failed = List.filter (fun (threshold, version, expected) ->
      at_least threshold version <> expected) cases in
  List.iter (fun (threshold, version, expected) ->
      Printf.eprintf "select_by_version: %s is %sat least %s\n"
        version (if expected then "" else "not ") threshold)
    failed;
  exit (if failed = [] then 0 else 1)

let () =
  match Array.to_list Sys.argv with
  | [_; "--self-test"] -> self_test ()
  | [_; threshold; from; before] ->
    cat (if at_least threshold Sys.ocaml_version then from else before)
  | [_; threshold; from; before; version] ->
    cat (if at_least threshold version then from else before)
  | _ ->
    prerr_endline "usage: ocaml -noinit select_by_version.ml MAJOR.MINOR FROM BEFORE [VERSION]";
    exit 2
