open OUnit2

let next_name =
  let i = ref 0 in
  fun () ->
    let name = "<anon prop> " ^ (string_of_int !i) in
    incr i;
    name

let to_ounit_test ?msg t =
  let msg =
    match msg with
    | Some m -> m
    | None ->
      begin match QCheck.name t with
      | None -> next_name ()
      | Some m -> m
      end in
  msg >:: (fun _ -> assert_bool msg (QCheck.run t))


let to_ounit_suite = List.map to_ounit_test

let (>:::) name tests = name >::: (to_ounit_suite tests)

let (~::) = to_ounit_test ?msg:None
