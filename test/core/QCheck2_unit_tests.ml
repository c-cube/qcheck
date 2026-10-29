open QCheck2

let rand_init i = Random.State.make [|i|]

let rec repeated_success t =
  Tree.root t :: match Tree.children t () with
  | Seq.Nil -> []
  | Seq.Cons (t,_) -> repeated_success t

let repeated_failure t =
  Tree.root t :: match Tree.children t () with
  | Seq.Nil -> []
  | Seq.Cons (t,ts) -> Tree.root t :: (Seq.map Tree.root ts |> List.of_seq)

let ocaml_major_version =
  try
    let major_version_str = List.hd (String.split_on_char '.' Sys.ocaml_version) in
    int_of_string major_version_str
  with _ -> failwith ("Unknown OCaml version format: " ^ Sys.ocaml_version)

module Shrink = struct
  let test_int_towards () =
    Alcotest.(check' (list int))
      ~msg:"int_towards 0 100"
      ~actual:(Shrink.int_towards 0 100 |> List.of_seq)
      ~expected:[0; 50; 75; 88; 94; 97; 99];
    Alcotest.(check' (list int))
      ~msg:"int_towards 500 1000"
      ~actual:(Shrink.int_towards 500 1000 |> List.of_seq)
      ~expected:[500; 750; 875; 938; 969; 985; 993; 997; 999];
    Alcotest.(check' (list int))
      ~msg:"int_towards (-50) (-26)"
      ~actual:(Shrink.int_towards (-50) (-26) |> List.of_seq)
      ~expected:[-50; -38; -32; -29; -28; -27]

  let test_int32_towards () =
    Alcotest.(check' (list int32))
      ~msg:"int32_towards 0l 100l"
      ~actual:(Shrink.int32_towards 0l 100l |> List.of_seq)
      ~expected:[0l; 50l; 75l; 88l; 94l; 97l; 99l];
    Alcotest.(check' (list int32))
      ~msg:"int32_towards 500l 1000l"
      ~actual:(Shrink.int32_towards 500l 1000l |> List.of_seq)
      ~expected:[500l; 750l; 875l; 938l; 969l; 985l; 993l; 997l; 999l];
    Alcotest.(check' (list int32))
      ~msg:"int32_towards (-50l) (-26l)"
      ~actual:(Shrink.int32_towards (-50l) (-26l) |> List.of_seq)
      ~expected:[-50l; -38l; -32l; -29l; -28l; -27l]

  let test_int64_towards () =
    Alcotest.(check' (list int64))
      ~msg:"int64_towards 0L 100L"
      ~actual:(Shrink.int64_towards 0L 100L |> List.of_seq)
      ~expected:[0L; 50L; 75L; 88L; 94L; 97L; 99L];
    Alcotest.(check' (list int64))
      ~msg:"int64_towards 500L 1000L"
      ~actual:(Shrink.int64_towards 500L 1000L |> List.of_seq)
      ~expected:[500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L];
    Alcotest.(check' (list int64))
      ~msg:"int64_towards (-50L) (-26L)"
      ~actual:(Shrink.int64_towards (-50L) (-26L) |> List.of_seq)
      ~expected:[-50L; -38L; -32L; -29L; -28L; -27L]

  let test_float_towards () =
    Alcotest.(check' (list (float 0.0001)))
      ~msg:"float_towards 0. 100."
      ~actual:(Shrink.float_towards 0. 100. |> List.of_seq)
      ~expected:[0.; 50.; 75.; 87.5; 93.75; 96.875; 98.4375; 99.2188; 99.6094; 99.8047; 99.9023; 99.9512; 99.9756; 99.9878; 99.9939];
    Alcotest.(check' (list (float 0.001)))
      ~msg:"float_towards 500. 1000."
      ~actual:(Shrink.float_towards 500. 1000. |> List.of_seq)
      ~expected:[500.; 750.; 875.; 937.5; 968.75; 984.375; 992.188; 996.094; 998.047; 999.023; 999.512; 999.756; 999.878; 999.939; 999.969];
    Alcotest.(check' (list (float 0.0001)))
      ~msg:"float_towards (-50.) (-26.)"
      ~actual:(Shrink.float_towards (-50.) (-26.) |> List.of_seq)
      ~expected:[-50.; -38.; -32.; -29.; -27.5; -26.75; -26.375; -26.1875; -26.0938; -26.0469; -26.0234; -26.0117; -26.0059; -26.0029; -26.0015]

  let test_char () =
    Alcotest.(check' (list char))
    ~msg:"'k' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['k'; 'a'; 'f'; 'h'; 'i'; 'j'] else ['>'; 'a'; 'P'; 'G'; 'C'; 'A'; '@'; '?']);
    Alcotest.(check' (list char))
    ~msg:"'1' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) char) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['1'; 'a'; 'I'; '='; '7'; '4'; '2'] else ['O'; 'a'; 'X'; 'S'; 'Q'; 'P']);
    Alcotest.(check' (list char))
    ~msg:"'k' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['k'; 'a';] else ['>'; 'a']);
    Alcotest.(check' (list char))
    ~msg:"'1' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) char) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['1'; 'a';] else ['O'; 'a'])

  let test_char_numeral () =
    Alcotest.(check' (list char))
    ~msg:"'3' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char_numeral) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['3'; '0'; '1'; '2'] else ['0']);
    Alcotest.(check' (list char))
    ~msg:"'0' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) char_numeral) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['0'] else ['9'; '0'; '4'; '6'; '7'; '8']);
    Alcotest.(check' (list char))
    ~msg:"'3' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char_numeral) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['3'; '0'] else ['0']);
    Alcotest.(check' (list char))
    ~msg:"'0' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) char_numeral) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['0'] else ['9'; '0'])

  let test_char_printable () =
    Alcotest.(check' (list char))
    ~msg:"'l' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char_printable) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['l'; 'a'; 'f'; 'i'; 'j'; 'k'] else ['D'; 'a'; '%'; '5'; '='; 'A'; 'C']);
    Alcotest.(check' (list char))
    ~msg:"'8' on repeated failure"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) char_printable) |> repeated_failure)
    ~expected:(if ocaml_major_version < 5 then ['8'; 'a'; 'z'; ','; '2'; '5'; '7'] else ['#'; 'a'; 'o'; 'v'; 'z'; '!'; '"']);
    Alcotest.(check' (list char))
    ~msg:"'l' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 1234) char_printable) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['l'; 'a'] else ['D'; 'a']);
    Alcotest.(check' (list char))
    ~msg:"'8' on repeated success"
    ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) char_printable) |> repeated_success)
    ~expected:(if ocaml_major_version < 5 then ['8'; 'a'] else ['#'; 'a'])

  let test_pair_nat_small () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (pair int int)))
         ~msg:"9,1 on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (pair nat_small nat_small)) |> repeated_failure)
         ~expected:[(9, 1); (0, 1); (4, 1); (6, 1); (7, 1); (8, 1); (9, 0)];
       Alcotest.(check' (list (pair int int)))
         ~msg:"9,1 on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (pair nat_small nat_small)) |> repeated_success)
         ~expected:[(9, 1); (0, 1); (0, 0)])
    else
      (Alcotest.(check' (list (pair int int)))
         ~msg:"2,6 on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (pair nat_small nat_small)) |> repeated_failure)
         ~expected:[(2, 6); (0, 6); (1, 6); (2, 0); (2, 3); (2, 5)];
       Alcotest.(check' (list (pair int int)))
         ~msg:"2,6 on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (pair nat_small nat_small)) |> repeated_success)
         ~expected:[(2, 6); (0, 6); (0, 0)])

  let test_bind_nat_small () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (pair int int)))
         ~msg:"9,1 on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (nat_small >>= fun i -> map (fun j -> (i,j)) nat_small)) |> repeated_failure)
         ~expected:[(9, 1); (0, 1); (4, 1); (6, 1); (7, 1); (8, 1); (9, 0)];
       Alcotest.(check' (list (pair int int)))
         ~msg:"9,1 on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (nat_small >>= fun i -> map (fun j -> (i,j)) nat_small)) |> repeated_success)
         ~expected:[(9, 1); (0, 1); (0, 0)])
    else
      (Alcotest.(check' (list (pair int int)))
         ~msg:"2,6 on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (nat_small >>= fun i -> map (fun j -> (i,j)) nat_small)) |> repeated_failure)
         ~expected:[(2, 6); (0, 6); (1, 6); (2, 0); (2, 3); (2, 5)];
       Alcotest.(check' (list (pair int int)))
         ~msg:"2,6 on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (nat_small >>= fun i -> map (fun j -> (i,j)) nat_small)) |> repeated_success)
         ~expected:[(2, 6); (0, 6); (0, 0)])

  let test_list_size_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (list int)))
         ~msg:"[8; 0; 9; 0; 8; 8; 2] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (list_size (int_bound 8) (int_bound 10))) |> repeated_failure)
         ~expected:[ [8; 0; 9; 0; 8; 8; 2]; []; [8; 0; 9]; [8; 0; 9; 0; 8]; [8; 0; 9; 0; 8; 8];
                     [0; 0; 9; 0; 8; 8; 2]; [4; 0; 9; 0; 8; 8; 2]; [6; 0; 9; 0; 8; 8; 2]; [7; 0; 9; 0; 8; 8; 2];
                     [8; 0; 0; 0; 8; 8; 2]; [8; 0; 4; 0; 8; 8; 2]; [8; 0; 6; 0; 8; 8; 2]; [8; 0; 7; 0; 8; 8; 2]; [8; 0; 8; 0; 8; 8; 2];
                     [8; 0; 9; 0; 0; 8; 2]; [8; 0; 9; 0; 4; 8; 2]; [8; 0; 9; 0; 6; 8; 2]; [8; 0; 9; 0; 7; 8; 2];
                     [8; 0; 9; 0; 8; 0; 2]; [8; 0; 9; 0; 8; 4; 2]; [8; 0; 9; 0; 8; 6; 2]; [8; 0; 9; 0; 8; 7; 2];
                     [8; 0; 9; 0; 8; 8; 0]; [8; 0; 9; 0; 8; 8; 1]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[8; 0; 9; 0; 8; 8; 2] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (list_size (int_bound 8) (int_bound 10))) |> repeated_success)
         ~expected:[ [8; 0; 9; 0; 8; 8; 2]; []; ])
    else
      (Alcotest.(check' (list (list int)))
         ~msg:"[9; 2; 7; 3; 8; 6] repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (list_size (int_bound 8) (int_bound 10))) |> repeated_failure)
         ~expected:[ [9; 2; 7; 3; 8; 6]; []; [9; 2; 7]; [9; 2; 7; 3; 8];
                     [0; 2; 7; 3; 8; 6]; [4; 2; 7; 3; 8; 6]; [6; 2; 7; 3; 8; 6]; [7; 2; 7; 3; 8; 6]; [8; 2; 7; 3; 8; 6];
                     [9; 0; 7; 3; 8; 6]; [9; 1; 7; 3; 8; 6];
                     [9; 2; 0; 3; 8; 6]; [9; 2; 3; 3; 8; 6]; [9; 2; 5; 3; 8; 6]; [9; 2; 6; 3; 8; 6];
                     [9; 2; 7; 0; 8; 6]; [9; 2; 7; 1; 8; 6]; [9; 2; 7; 2; 8; 6];
                     [9; 2; 7; 3; 0; 6]; [9; 2; 7; 3; 4; 6]; [9; 2; 7; 3; 6; 6]; [9; 2; 7; 3; 7; 6];
                     [9; 2; 7; 3; 8; 0]; [9; 2; 7; 3; 8; 3]; [9; 2; 7; 3; 8; 5]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[9; 2; 7; 3; 8; 6] repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (list_size (int_bound 8) (int_bound 10))) |> repeated_success)
         ~expected:[[9; 2; 7; 3; 8; 6]; []; ])

  let test_list_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (list int)))
         ~msg:"[0; 5; 3; 7] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (list (int_bound 10))) |> repeated_failure)
         ~expected:[ [0; 5; 3; 7]; [0; 5]; [3; 7]; [5; 3; 7]; [0; 5; 7];
                     [0; 0; 3; 7]; [0; 2; 3; 7]; [0; 3; 3; 7]; [0; 4; 3; 7];
                     [0; 5; 0; 7]; [0; 5; 1; 7]; [0; 5; 2; 7];
                     [0; 5; 3; 0]; [0; 5; 3; 3]; [0; 5; 3; 5]; [0; 5; 3; 6]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[0; 5; 3; 7] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (list (int_bound 10))) |> repeated_success)
         ~expected:[ [0; 5; 3; 7]; [0; 5]; [5]; []; ])
    else
      (Alcotest.(check' (list (list int)))
         ~msg:"[1; 10; 10; 7; 3] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (list (int_bound 10))) |> repeated_failure)
         ~expected:[ [1; 10; 10; 7; 3]; [1; 10; 10]; [7; 3]; [10; 10; 7; 3]; [1; 10; 10; 3];
                     [0; 10; 10; 7; 3];
                     [1; 0; 10; 7; 3]; [1; 5; 10; 7; 3]; [1; 8; 10; 7; 3]; [1; 9; 10; 7; 3];
                     [1; 10; 0; 7; 3]; [1; 10; 5; 7; 3]; [1; 10; 8; 7; 3]; [1; 10; 9; 7; 3];
                     [1; 10; 10; 0; 3]; [1; 10; 10; 3; 3]; [1; 10; 10; 5; 3]; [1; 10; 10; 6; 3];
                     [1; 10; 10; 7; 0]; [1; 10; 10; 7; 1]; [1; 10; 10; 7; 2]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[1; 10; 10; 7; 3] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (list (int_bound 10))) |> repeated_success)
         ~expected:[ [1; 10; 10; 7; 3]; [1; 10; 10]; [10; 10]; [10]; []; ])

  let test_list_small_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (list int)))
         ~msg:"[0; 5; 3; 7] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (list_small (int_bound 10))) |> repeated_failure)
         ~expected:[[0; 5; 3; 7]; [0; 5]; [3; 7]; [5; 3; 7]; [0; 5; 7];
                    [0; 0; 3; 7]; [0; 2; 3; 7]; [0; 3; 3; 7]; [0; 4; 3; 7];
                    [0; 5; 0; 7]; [0; 5; 1; 7]; [0; 5; 2; 7];
                    [0; 5; 3; 0]; [0; 5; 3; 3]; [0; 5; 3; 5]; [0; 5; 3; 6]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[0; 5; 3; 7] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (list_small (int_bound 10))) |> repeated_success)
         ~expected:[ [0; 5; 3; 7]; [0; 5]; [5]; []; ])
    else
      (Alcotest.(check' (list (list int)))
         ~msg:"[1; 10; 10; 7; 3] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (list_small (int_bound 10))) |> repeated_failure)
         ~expected:[ [1; 10; 10; 7; 3]; [1; 10; 10]; [7; 3]; [10; 10; 7; 3]; [1; 10; 10; 3];
                     [0; 10; 10; 7; 3];
                     [1; 0; 10; 7; 3]; [1; 5; 10; 7; 3]; [1; 8; 10; 7; 3]; [1; 9; 10; 7; 3];
                     [1; 10; 0; 7; 3]; [1; 10; 5; 7; 3]; [1; 10; 8; 7; 3]; [1; 10; 9; 7; 3];
                     [1; 10; 10; 0; 3]; [1; 10; 10; 3; 3]; [1; 10; 10; 5; 3]; [1; 10; 10; 6; 3];
                     [1; 10; 10; 7; 0]; [1; 10; 10; 7; 1]; [1; 10; 10; 7; 2]; ];
       Alcotest.(check' (list (list int)))
         ~msg:"[1; 10; 10; 7; 3] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (list_small (int_bound 10))) |> repeated_success)
         ~expected:[ [1; 10; 10; 7; 3]; [1; 10; 10]; [10; 10]; [10]; []; ])

  let test_array_size_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (array int)))
         ~msg:"[|2; 5; 1|] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array_size (int_bound 8) (int_bound 10))) |> repeated_failure)
         ~expected:[ [|2; 5; 1|]; [||]; [|2|]; [|2; 5|];
                     [|0; 5; 1|]; [|1; 5; 1|];
                     [|2; 0; 1|]; [|2; 2; 1|]; [|2; 3; 1|]; [|2; 4; 1|];
                     [|2; 5; 0|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|2; 5; 1|] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array_size (int_bound 8) (int_bound 10))) |> repeated_success)
         ~expected:[ [|2; 5; 1|]; [| |]; ])
    else
      (Alcotest.(check' (list (array int)))
         ~msg:"[|9; 2; 7; 3; 8; 6|] repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (array_size (int_bound 8) (int_bound 10))) |> repeated_failure)
         ~expected:[ [|9; 2; 7; 3; 8; 6|]; [||]; [|9; 2; 7|]; [|9; 2; 7; 3; 8|];
                     [|0; 2; 7; 3; 8; 6|]; [|4; 2; 7; 3; 8; 6|]; [|6; 2; 7; 3; 8; 6|]; [|7; 2; 7; 3; 8; 6|]; [|8; 2; 7; 3; 8; 6|];
                     [|9; 0; 7; 3; 8; 6|]; [|9; 1; 7; 3; 8; 6|];
                     [|9; 2; 0; 3; 8; 6|]; [|9; 2; 3; 3; 8; 6|]; [|9; 2; 5; 3; 8; 6|]; [|9; 2; 6; 3; 8; 6|];
                     [|9; 2; 7; 0; 8; 6|]; [|9; 2; 7; 1; 8; 6|]; [|9; 2; 7; 2; 8; 6|];
                     [|9; 2; 7; 3; 0; 6|]; [|9; 2; 7; 3; 4; 6|]; [|9; 2; 7; 3; 6; 6|]; [|9; 2; 7; 3; 7; 6|];
                     [|9; 2; 7; 3; 8; 0|]; [|9; 2; 7; 3; 8; 3|]; [|9; 2; 7; 3; 8; 5|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|9; 2; 7; 3; 8; 6|] repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (array_size (int_bound 8) (int_bound 10))) |> repeated_success)
         ~expected:[[|9; 2; 7; 3; 8; 6|]; [| |]; ])

  let test_array_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (array int)))
         ~msg:"[|0; 5; 3; 7|] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (array (int_bound 10))) |> repeated_failure)
         ~expected:[ [|0; 5; 3; 7|]; [|0; 5|]; [|3; 7|]; [|5; 3; 7|]; [|0; 5; 7|];
                     [|0; 0; 3; 7|]; [|0; 2; 3; 7|]; [|0; 3; 3; 7|]; [|0; 4; 3; 7|];
                     [|0; 5; 0; 7|]; [|0; 5; 1; 7|]; [|0; 5; 2; 7|];
                     [|0; 5; 3; 0|]; [|0; 5; 3; 3|]; [|0; 5; 3; 5|]; [|0; 5; 3; 6|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|0; 5; 3; 7|] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (array (int_bound 10))) |> repeated_success)
         ~expected:[ [|0; 5; 3; 7|]; [|0; 5|]; [|5|]; [||]; ])
    else
      (Alcotest.(check' (list (array int)))
         ~msg:"[|1; 10; 10; 7; 3|] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array (int_bound 10))) |> repeated_failure)
         ~expected:[ [|1; 10; 10; 7; 3|]; [|1; 10; 10|]; [|7; 3|]; [|10; 10; 7; 3|]; [|1; 10; 10; 3|];
                     [|0; 10; 10; 7; 3|];
                     [|1; 0; 10; 7; 3|]; [|1; 5; 10; 7; 3|]; [|1; 8; 10; 7; 3|]; [|1; 9; 10; 7; 3|];
                     [|1; 10; 0; 7; 3|]; [|1; 10; 5; 7; 3|]; [|1; 10; 8; 7; 3|]; [|1; 10; 9; 7; 3|];
                     [|1; 10; 10; 0; 3|]; [|1; 10; 10; 3; 3|]; [|1; 10; 10; 5; 3|]; [|1; 10; 10; 6; 3|];
                     [|1; 10; 10; 7; 0|]; [|1; 10; 10; 7; 1|]; [|1; 10; 10; 7; 2|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|1; 10; 10; 7; 3|] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array (int_bound 10))) |> repeated_success)
         ~expected:[ [|1; 10; 10; 7; 3|]; [|1; 10; 10|]; [|10; 10|]; [|10|]; [||]; ])

  let test_array_small_int () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list (array int)))
         ~msg:"[|0; 5; 3; 7|] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (array_small (int_bound 10))) |> repeated_failure)
         ~expected:[ [|0; 5; 3; 7|]; [|0; 5|]; [|3; 7|]; [|5; 3; 7|]; [|0; 5; 7|];
                     [|0; 0; 3; 7|]; [|0; 2; 3; 7|]; [|0; 3; 3; 7|]; [|0; 4; 3; 7|];
                     [|0; 5; 0; 7|]; [|0; 5; 1; 7|]; [|0; 5; 2; 7|];
                     [|0; 5; 3; 0|]; [|0; 5; 3; 3|]; [|0; 5; 3; 5|]; [|0; 5; 3; 6|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|0; 5; 3; 7|] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) (array_small (int_bound 10))) |> repeated_success)
         ~expected:[ [|0; 5; 3; 7|]; [|0; 5|]; [|5|]; [||]; ])
    else
      (Alcotest.(check' (list (array int)))
         ~msg:"[|1; 10; 10; 7; 3|] on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array_small (int_bound 10))) |> repeated_failure)
         ~expected:[ [|1; 10; 10; 7; 3|]; [|1; 10; 10|]; [|7; 3|]; [|10; 10; 7; 3|]; [|1; 10; 10; 3|];
                     [|0; 10; 10; 7; 3|];
                     [|1; 0; 10; 7; 3|]; [|1; 5; 10; 7; 3|]; [|1; 8; 10; 7; 3|]; [|1; 9; 10; 7; 3|];
                     [|1; 10; 0; 7; 3|]; [|1; 10; 5; 7; 3|]; [|1; 10; 8; 7; 3|]; [|1; 10; 9; 7; 3|];
                     [|1; 10; 10; 0; 3|]; [|1; 10; 10; 3; 3|]; [|1; 10; 10; 5; 3|]; [|1; 10; 10; 6; 3|];
                     [|1; 10; 10; 7; 0|]; [|1; 10; 10; 7; 1|]; [|1; 10; 10; 7; 2|]; ];
       Alcotest.(check' (list (array int)))
         ~msg:"[|1; 10; 10; 7; 3|] on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3347) (array_small (int_bound 10))) |> repeated_success)
         ~expected:[ [|1; 10; 10; 7; 3|]; [|1; 10; 10|]; [|10; 10|]; [|10|]; [||]; ])

  let test_bytes_size_of () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list bytes))
         ~msg:"\"H Ap>&U\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (bytes_size_of (int_bound 8) char_printable)) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      [ "H Ap>&U"; ""; "H A"; "H Ap>"; "H Ap>&"; "a Ap>&U"; "' Ap>&U";
                        "8 Ap>&U"; "@ Ap>&U"; "D Ap>&U"; "F Ap>&U"; "G Ap>&U";
                        "HaAp>&U"; "HnAp>&U"; "HuAp>&U"; "HxAp>&U"; "HzAp>&U";
                        "H ap>&U"; "H #p>&U"; "H 2p>&U"; "H 9p>&U"; "H =p>&U"; "H ?p>&U"; "H @p>&U";
                        "H Aa>&U"; "H Ah>&U"; "H Al>&U"; "H An>&U"; "H Ao>&U";
                        "H Apa&U"; "H Ap\"&U"; "H Ap0&U"; "H Ap7&U"; "H Ap;&U"; "H Ap=&U";
                        "H Ap>aU"; "H Ap>qU"; "H Ap>yU"; "H Ap>\"U"; "H Ap>$U"; "H Ap>%U";
                        "H Ap>&a"; "H Ap>&-"; "H Ap>&A"; "H Ap>&K"; "H Ap>&P"; "H Ap>&R"; "H Ap>&S"; "H Ap>&T"; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"H Ap>&U\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (bytes_size_of (int_bound 8) char_printable)) |> repeated_success)
         ~expected:(List.map Bytes.of_string ["H Ap>&U"; ""]))
    else
      (Alcotest.(check' (list bytes))
         ~msg:"\"Ns<>W\\\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (bytes_size_of (int_bound 8) char_printable)) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      ["Ns<>W\\"; ""; "Ns<"; "Ns<>W";
                       "as<>W\\"; "*s<>W\\"; "<s<>W\\"; "Es<>W\\"; "Js<>W\\"; "Ls<>W\\"; "Ms<>W\\";
                       "Na<>W\\"; "Nj<>W\\"; "No<>W\\"; "Nq<>W\\"; "Nr<>W\\";
                       "Nsa>W\\"; "Ns!>W\\"; "Ns/>W\\"; "Ns6>W\\"; "Ns9>W\\"; "Ns;>W\\";
                       "Ns<aW\\"; "Ns<\"W\\"; "Ns<0W\\"; "Ns<7W\\"; "Ns<;W\\"; "Ns<=W\\";
                       "Ns<>a\\"; "Ns<>.\\"; "Ns<>B\\"; "Ns<>L\\"; "Ns<>Q\\"; "Ns<>T\\"; "Ns<>U\\"; "Ns<>V\\";
                       "Ns<>Wa"; "Ns<>W1"; "Ns<>WG"; "Ns<>WR"; "Ns<>WW"; "Ns<>WZ"; "Ns<>W["; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"Ns<>W\\\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (bytes_size_of (int_bound 8) char_printable)) |> repeated_success)
         ~expected:(List.map Bytes.of_string ["Ns<>W\\"; ""; ]))

  let test_bytes () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list bytes))
         ~msg:"\"9\007\127\250\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) bytes) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      [ "9\007\127\250"; "9\007"; "\127\250"; "\007\127\250"; "9\007\250";
                        "a\007\127\250"; "M\007\127\250"; "C\007\127\250"; ">\007\127\250"; ";\007\127\250"; ":\007\127\250";
                        "9a\127\250"; "94\127\250"; "9\029\127\250"; "9\018\127\250"; "9\012\127\250"; "9\t\127\250"; "9\b\127\250";
                        "9\007a\250"; "9\007p\250"; "9\007w\250"; "9\007{\250"; "9\007}\250"; "9\007~\250";
                        "9\007\127a"; "9\007\127\174"; "9\007\127\212"; "9\007\127\231"; "9\007\127\241"; "9\007\127\246"; "9\007\127\248"; "9\007\127\249"; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"9\007\127\250\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) bytes) |> repeated_success)
         ~expected:(List.map Bytes.of_string [ "9\007\127\250"; "9\007"; "\007"; ""; ]))
    else
      (Alcotest.(check' (list bytes))
         ~msg:"\"\253NS\173\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) bytes) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      [ "\253NS\173"; "\253N"; "S\173"; "NS\173"; "\253N\173";
                        "aNS\173"; "\175NS\173"; "\214NS\173"; "\233NS\173"; "\243NS\173"; "\248NS\173"; "\250NS\173"; "\251NS\173"; "\252NS\173";
                        "\253aS\173"; "\253XS\173"; "\253SS\173"; "\253QS\173"; "\253PS\173"; "\253OS\173";
                        "\253Na\173"; "\253NZ\173"; "\253NV\173"; "\253NT\173";
                        "\253NSa"; "\253NS\135"; "\253NS\154"; "\253NS\163"; "\253NS\168"; "\253NS\170"; "\253NS\171"; "\253NS\172"; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"\253NS\173\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) bytes) |> repeated_success)
         ~expected:(List.map Bytes.of_string [ "\253NS\173"; "\253N"; "N"; ""; ]))

  let test_bytes_small () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list bytes))
         ~msg:"\"9\007\127\250\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) bytes_small) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      [ "9\007\127\250"; "9\007"; "\127\250"; "\007\127\250"; "9\007\250";
                        "a\007\127\250"; "M\007\127\250"; "C\007\127\250"; ">\007\127\250"; ";\007\127\250"; ":\007\127\250";
                        "9a\127\250"; "94\127\250"; "9\029\127\250"; "9\018\127\250"; "9\012\127\250"; "9\t\127\250"; "9\b\127\250";
                        "9\007a\250"; "9\007p\250"; "9\007w\250"; "9\007{\250"; "9\007}\250"; "9\007~\250";
                        "9\007\127a"; "9\007\127\174"; "9\007\127\212"; "9\007\127\231"; "9\007\127\241"; "9\007\127\246"; "9\007\127\248"; "9\007\127\249"; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"9\007\127\250\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) bytes_small) |> repeated_success)
         ~expected:(List.map Bytes.of_string [ "9\007\127\250"; "9\007"; "\007"; ""; ]))
    else
      (Alcotest.(check' (list bytes))
         ~msg:"\"\253NS\173\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) bytes_small) |> repeated_failure)
         ~expected:(List.map Bytes.of_string
                      [ "\253NS\173"; "\253N"; "S\173"; "NS\173"; "\253N\173";
                        "aNS\173"; "\175NS\173"; "\214NS\173"; "\233NS\173"; "\243NS\173"; "\248NS\173"; "\250NS\173"; "\251NS\173"; "\252NS\173";
                        "\253aS\173"; "\253XS\173"; "\253SS\173"; "\253QS\173"; "\253PS\173"; "\253OS\173";
                        "\253Na\173"; "\253NZ\173"; "\253NV\173"; "\253NT\173";
                        "\253NSa"; "\253NS\135"; "\253NS\154"; "\253NS\163"; "\253NS\168"; "\253NS\170"; "\253NS\171"; "\253NS\172"; ] );
       Alcotest.(check' (list bytes))
         ~msg:"\"\253NS\173\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) bytes_small) |> repeated_success)
         ~expected:(List.map Bytes.of_string [ "\253NS\173"; "\253N"; "N"; ""; ]))

  let test_string_size_of () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list string))
         ~msg:"\"H Ap>&U\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (string_size_of (int_bound 8) char_printable)) |> repeated_failure)
         ~expected:[ "H Ap>&U"; ""; "H A"; "H Ap>"; "H Ap>&"; "a Ap>&U"; "' Ap>&U";
                     "8 Ap>&U"; "@ Ap>&U"; "D Ap>&U"; "F Ap>&U"; "G Ap>&U";
                     "HaAp>&U"; "HnAp>&U"; "HuAp>&U"; "HxAp>&U"; "HzAp>&U";
                     "H ap>&U"; "H #p>&U"; "H 2p>&U"; "H 9p>&U"; "H =p>&U"; "H ?p>&U"; "H @p>&U";
                     "H Aa>&U"; "H Ah>&U"; "H Al>&U"; "H An>&U"; "H Ao>&U";
                     "H Apa&U"; "H Ap\"&U"; "H Ap0&U"; "H Ap7&U"; "H Ap;&U"; "H Ap=&U";
                     "H Ap>aU"; "H Ap>qU"; "H Ap>yU"; "H Ap>\"U"; "H Ap>$U"; "H Ap>%U";
                     "H Ap>&a"; "H Ap>&-"; "H Ap>&A"; "H Ap>&K"; "H Ap>&P"; "H Ap>&R"; "H Ap>&S"; "H Ap>&T"; ];
       Alcotest.(check' (list string))
         ~msg:"\"H Ap>&U\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (string_size_of (int_bound 8) char_printable)) |> repeated_success)
         ~expected:[ "H Ap>&U"; ""; ])
    else
      (Alcotest.(check' (list string))
         ~msg:"\"Ns<>W\\\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (string_size_of (int_bound 8) char_printable)) |> repeated_failure)
         ~expected:["Ns<>W\\"; ""; "Ns<"; "Ns<>W"; "as<>W\\"; "*s<>W\\";
                    "<s<>W\\"; "Es<>W\\"; "Js<>W\\"; "Ls<>W\\"; "Ms<>W\\";
                    "Na<>W\\"; "Nj<>W\\"; "No<>W\\"; "Nq<>W\\"; "Nr<>W\\";
                    "Nsa>W\\"; "Ns!>W\\"; "Ns/>W\\"; "Ns6>W\\"; "Ns9>W\\"; "Ns;>W\\";
                    "Ns<aW\\"; "Ns<\"W\\"; "Ns<0W\\"; "Ns<7W\\"; "Ns<;W\\"; "Ns<=W\\";
                    "Ns<>a\\"; "Ns<>.\\"; "Ns<>B\\"; "Ns<>L\\"; "Ns<>Q\\"; "Ns<>T\\"; "Ns<>U\\"; "Ns<>V\\";
                    "Ns<>Wa"; "Ns<>W1"; "Ns<>WG"; "Ns<>WR"; "Ns<>WW"; "Ns<>WZ"; "Ns<>W["; ];
       Alcotest.(check' (list string))
         ~msg:"\"Ns<>W\\\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3346) (string_size_of (int_bound 8) char_printable)) |> repeated_success)
         ~expected:[ "Ns<>W\\"; ""; ])

  let test_string () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list string))
         ~msg:"\"9\007\127\250\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) string) |> repeated_failure)
         ~expected:[ "9\007\127\250"; "9\007"; "\127\250"; "\007\127\250"; "9\007\250";
                     "a\007\127\250"; "M\007\127\250"; "C\007\127\250"; ">\007\127\250"; ";\007\127\250"; ":\007\127\250";
                     "9a\127\250"; "94\127\250"; "9\029\127\250"; "9\018\127\250"; "9\012\127\250"; "9\t\127\250"; "9\b\127\250";
                     "9\007a\250"; "9\007p\250"; "9\007w\250"; "9\007{\250"; "9\007}\250"; "9\007~\250";
                     "9\007\127a"; "9\007\127\174"; "9\007\127\212"; "9\007\127\231"; "9\007\127\241"; "9\007\127\246"; "9\007\127\248"; "9\007\127\249"; ];
       Alcotest.(check' (list string))
         ~msg:"\"9\007\127\250\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) string) |> repeated_success)
         ~expected:[ "9\007\127\250"; "9\007"; "\007"; ""; ])
    else
      (Alcotest.(check' (list string))
         ~msg:"\"\253NS\173\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) string) |> repeated_failure)
         ~expected:[ "\253NS\173"; "\253N"; "S\173"; "NS\173"; "\253N\173";
                     "aNS\173"; "\175NS\173"; "\214NS\173"; "\233NS\173"; "\243NS\173"; "\248NS\173"; "\250NS\173"; "\251NS\173"; "\252NS\173";
                     "\253aS\173"; "\253XS\173"; "\253SS\173"; "\253QS\173"; "\253PS\173"; "\253OS\173";
                     "\253Na\173"; "\253NZ\173"; "\253NV\173"; "\253NT\173";
                     "\253NSa"; "\253NS\135"; "\253NS\154"; "\253NS\163"; "\253NS\168"; "\253NS\170"; "\253NS\171"; "\253NS\172"];
       Alcotest.(check' (list string))
         ~msg:"\"\253NS\173\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) string) |> repeated_success)
         ~expected:[ "\253NS\173"; "\253N"; "N"; ""; ])

  let test_string_small () =
    if ocaml_major_version < 5
    then
      (Alcotest.(check' (list string))
         ~msg:"\"9\007\127\250\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) string_small) |> repeated_failure)
         ~expected:[ "9\007\127\250"; "9\007"; "\127\250"; "\007\127\250"; "9\007\250";
                     "a\007\127\250"; "M\007\127\250"; "C\007\127\250"; ">\007\127\250"; ";\007\127\250"; ":\007\127\250";
                     "9a\127\250"; "94\127\250"; "9\029\127\250"; "9\018\127\250"; "9\012\127\250"; "9\t\127\250"; "9\b\127\250";
                     "9\007a\250"; "9\007p\250"; "9\007w\250"; "9\007{\250"; "9\007}\250"; "9\007~\250";
                     "9\007\127a"; "9\007\127\174"; "9\007\127\212"; "9\007\127\231"; "9\007\127\241"; "9\007\127\246"; "9\007\127\248"; "9\007\127\249"; ];
       Alcotest.(check' (list string))
         ~msg:"\"u\238\154I\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3345) string_small) |> repeated_success)
         ~expected:[ "9\007\127\250"; "9\007"; "\007"; ""; ])
    else
      (Alcotest.(check' (list string))
         ~msg:"\"\253NS\173\" on repeated failure"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) string_small) |> repeated_failure)
         ~expected:[ "\253NS\173"; "\253N"; "S\173"; "NS\173"; "\253N\173";
                     "aNS\173"; "\175NS\173"; "\214NS\173"; "\233NS\173"; "\243NS\173"; "\248NS\173"; "\250NS\173"; "\251NS\173"; "\252NS\173";
                     "\253aS\173"; "\253XS\173"; "\253SS\173"; "\253QS\173"; "\253PS\173"; "\253OS\173";
                     "\253Na\173"; "\253NZ\173"; "\253NV\173"; "\253NT\173";
                     "\253NSa"; "\253NS\135"; "\253NS\154"; "\253NS\163"; "\253NS\168"; "\253NS\170"; "\253NS\171"; "\253NS\172"];
       Alcotest.(check' (list string))
         ~msg:"\"\253NS\173\" on repeated success"
         ~actual:(Gen.(generate_tree ~rand:(rand_init 3349) string_small) |> repeated_success)
         ~expected:[ "\253NS\173"; "\253N"; "N"; ""; ])

  let tests = ("Shrink", Alcotest.[
      test_case "int_towards" `Quick test_int_towards;
      test_case "int32_towards" `Quick test_int32_towards;
      test_case "int64_towards" `Quick test_int64_towards;
      test_case "float_towards" `Quick test_float_towards;
      test_case "Gen.char tree" `Quick test_char;
      test_case "Gen.char_numeral tree" `Quick test_char_numeral;
      test_case "Gen.char_printable tree" `Quick test_char_printable;
      test_case "Gen.(pair nat_small nat_small) tree" `Quick test_pair_nat_small;
      test_case "Gen.bind nat_small tree" `Quick test_bind_nat_small;
      test_case "Gen.list_size int" `Quick test_list_size_int;
      test_case "Gen.list int" `Quick test_list_int;
      test_case "Gen.list_small int" `Quick test_list_small_int;
      test_case "Gen.array_size int" `Quick test_array_size_int;
      test_case "Gen.array int" `Quick test_array_int;
      test_case "Gen.array_small int" `Quick test_array_small_int;
      test_case "Gen.bytes_size_of" `Quick test_bytes_size_of;
      test_case "Gen.bytes" `Quick test_bytes;
      test_case "Gen.bytes_small" `Quick test_bytes_small;
      test_case "Gen.string_size_of" `Quick test_string_size_of;
      test_case "Gen.string" `Quick test_string;
      test_case "Gen.string_small" `Quick test_string_small;
    ])
end

module Gen = struct
  let test_gen_option ~ratio =
    let opt_int = Gen.option ?ratio Gen.int in
    let nb = ref 0 in
    for _i = 0 to 1000 do
      Gen.generate1 opt_int |> function None -> () | Some _ -> nb := !nb + 1
    done;
    !nb

  let test_gen_option_default () =
    let nb = test_gen_option ~ratio:None in
    let b = nb > 800 && nb < 900 in
    Alcotest.(check bool) "Gen.option produces around 85% of Some" b true

  let test_gen_option_custom () =
    let nb = test_gen_option ~ratio:(Some 0.5) in
    let b = nb > 400 && nb < 600 in
    Alcotest.(check bool) "Gen.option produces around 50% of Some" b true

  let tests =
    ("Gen", Alcotest.[
         test_case "option with default ratio" `Quick test_gen_option_default;
         test_case "option with custom ratio" `Quick test_gen_option_custom;
       ])
end

module TestCount = struct
  let test_count_n ?count expected =
    let t = QCheck2.(Test.make ?count Gen.int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck2.Test.make ~count:%s |> get_count = %d"
                (Option.fold ~none:"None" ~some:string_of_int count) expected
    in
    Alcotest.(check int) msg expected (QCheck2.Test.test_get_count t)

  let test_count_10 () = test_count_n ~count:10 10

  let test_count_default () = test_count_n 100

  let test_count_env () =
    let () = Unix.putenv "QCHECK_COUNT" "5" in
    let t = QCheck2.(Test.make Gen.int (fun _ -> true)) in
    let actual = QCheck2.Test.test_get_count t in
    Alcotest.(check int) "default count is from QCHECK_COUNT" 5 actual

  let test_count_0 () = test_count_n ~count:0 0

  let test_count_negative_fail () =
    try
      let _ = test_count_n ~count:(-1) (-1) in
      Alcotest.fail "A negative count in a test should fail"
    with
    | _ -> ()

  (* Regression for https://github.com/c-cube/qcheck/issues/408:
     [make_cell] computed [max_gen = count + 200] without checking for
     overflow, so [count >= max_int - 199] wrapped to a negative int
     and [check_cell] silently ran zero iterations. *)
  let test_count_max_int_no_overflow () =
    let cell =
      QCheck2.(Test.make_cell ~name:"never_true" ~count:max_int
                 Gen.int (fun _ -> false))
    in
    let result =
      QCheck2.Test.check_cell ~rand:(Random.State.make [|0|]) cell
    in
    let tested = QCheck2.TestResult.get_count_gen result in
    Alcotest.(check bool)
      "check_cell with count=max_int runs at least one iteration"
      true (tested >= 1)

  let tests =
    ("Test.make ~count", Alcotest.[
         test_case "make with custom count" `Quick test_count_10;
         test_case "make with default count" `Quick test_count_default;
         test_case "make with env count" `Quick test_count_env;
         test_case "make with 0 count" `Quick test_count_0;
         test_case "make with negative count should fail"
           `Quick test_count_negative_fail;
         test_case "check_cell with count=max_int does not overflow"
           `Quick test_count_max_int_no_overflow;
       ])
end

module TestLongFactor = struct
  let test_long_factor_n ?long_factor expected =
    let t = QCheck2.(Test.make ?long_factor Gen.int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck2.Test.make ~long_factor:%s |> long_factor = %d"
                (Option.fold ~none:"None" ~some:string_of_int long_factor) expected
    in
    Alcotest.(check int) msg expected (QCheck2.Test.test_get_long_factor t)

  let test_long_factor_10 () = test_long_factor_n ~long_factor:10 10

  let test_long_factor_default () = test_long_factor_n 1

  let test_long_factor_env () =
    let () = Unix.putenv "QCHECK_LONG_FACTOR" "5" in
    let t = QCheck2.(Test.make Gen.int (fun _ -> true)) in
    let actual = QCheck2.Test.test_get_long_factor t in
    Alcotest.(check int) "default long factor is from QCHECK_LONG_FACTOR" 5 actual

  let test_long_factor_0 () = test_long_factor_n ~long_factor:0 0

  let test_long_factor_negative_fail () =
    try
      let _ = test_long_factor_n ~long_factor:(-1) (-1) in
      Alcotest.fail "A negative long factor in a test should fail"
    with
    | _ -> ()

  let tests =
    ("Test.make ~long_factor", Alcotest.[
         test_case "make with custom long_factor" `Quick test_long_factor_10;
         test_case "make with default long_factor" `Quick test_long_factor_default;
         test_case "make with env long_factor" `Quick test_long_factor_env;
         test_case "make with 0 long_factor" `Quick test_long_factor_0;
         test_case "make with negative long_factor fail should"
           `Quick test_long_factor_negative_fail;
       ])
end

module String = struct

  let test_string_shrinking () =
    let shrink_result = QCheck2.(find_example_gen ~f:(fun s -> s <> s ^ s) Gen.string) in
    Alcotest.(check string) "Shrinking a non-empty string shrinks to \"a\"" "a" shrink_result

  let tests = ("String", Alcotest.[test_case "shrinking" `Quick test_string_shrinking])
end

module Check_exn = struct

  (* String.starts_with was introduced in 4.13.
     Include the below to support pre-4.13 OCaml. *)
  let string_starts_with ~prefix s =
    let open Stdlib in
    let prefix_len = String.length prefix in
    prefix_len <= String.length s
    && prefix = String.sub s 0 prefix_len

  let check_exn = Test.check_exn

  let test_pass_trivial () =
    let run_test () = check_exn QCheck2.(Test.make Gen.int (fun _ -> true)) in
    Alcotest.(check unit) "Success-trivial" () @@ run_test ()

  let test_pass_random () =
    let run_test () =
      check_exn QCheck2.(Test.make Gen.(list int) (fun l -> List.rev (List.rev l) = l)) in
    Alcotest.(check unit) "Success-random" () @@ run_test ()

  let test_fail_always () =
    let name = "will-always-fail" in
    try
      check_exn QCheck2.(Test.make ~name ~print:Print.int Gen.int (fun _ -> false));
      Alcotest.failf "%s: Unexpected success" name
    with      
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received: \"%s\"" name c_ex_str

  let test_fail_random () =
    let name = "list is own reverse" in
    try
      check_exn
        QCheck2.(Test.make ~name ~print:Print.(list int)
                   Gen.(list int) (fun l -> List.rev l = l));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"[0; 1]" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  exception MyError

  let test_error () =
    let name = "will-always-error" in
    try
      Printexc.record_backtrace false; (* for easier pattern-matching below *)
      check_exn QCheck2.(Test.make ~name ~print:Print.int Gen.int (fun _ -> raise MyError));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_error (n,c_ex_str,MyError,"")) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  let test_negative_trivial () =
    let run_test () = check_exn QCheck2.(Test.make_neg Gen.int (fun _ -> false)) in
    Alcotest.(check unit) "Success-negative-trivial" () @@ run_test ()

  let test_negative_test_unexpected_success () =
    let name = "negative-trivial-test" in
    let run_test () = check_exn QCheck2.(Test.make_neg ~name Gen.int (fun _ -> true)) in
    try
      run_test ();
      Alcotest.failf "Negative test didn't raise expected exception."
    with
      Test.Test_unexpected_success n ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name

  let tests =
    ("Test.check_exn", Alcotest.[
         test_case "check_exn pass trivial" `Quick test_pass_trivial;
         test_case "check_exn pass random" `Quick test_pass_random;
         test_case "check_exn fail always" `Quick test_fail_always;
         test_case "check_exn fail random" `Quick test_fail_random;
         test_case "check_exn Error" `Quick test_error;
         test_case "check_exn negative pass trivial" `Quick test_negative_trivial;
         test_case "check_exn Unexpected success" `Quick test_negative_test_unexpected_success;
       ])
end

module Incomplete = struct
  open QCheck2

  let rand () = Random.State.make [|0|]

  let run ?(negative=false) ?if_assumptions_fail gen law =
    Test.check_cell ~rand:(rand ())
      (Test.make_cell ~count:100 ~negative ?if_assumptions_fail gen law)

  let test_all_incomplete () =
    let r = run Gen.int (fun _ -> incomplete "later") in
    let gen = TestResult.get_count_gen r in
    Alcotest.(check bool) "state is Success" true (TestResult.is_success r);
    Alcotest.(check int) "no case was decided" 0 (TestResult.get_count r);
    Alcotest.(check int) "every generated case is incomplete"
      gen (TestResult.get_count_incomplete r);
    Alcotest.(check (list (pair string int))) "reasons"
      ["later", gen] (TestResult.get_incomplete_reasons r);
    Alcotest.(check (list string)) "no precondition warning"
      [] (TestResult.get_warnings r)

  let test_several_reasons () =
    let r = run Gen.int (fun i -> match abs i mod 3 with
        | 0 -> incomplete "a"
        | 1 -> incomplete "b"
        | _ -> true) in
    let reasons = TestResult.get_incomplete_reasons r in
    Alcotest.(check (list string)) "both reasons"
      ["a"; "b"] (List.sort compare (List.map fst reasons));
    Alcotest.(check int) "counts add up"
      (TestResult.get_count_incomplete r)
      (List.fold_left (fun acc (_, n) -> acc + n) 0 reasons);
    Alcotest.(check bool) "sorted by decreasing count, then by reason" true
      (reasons = List.sort (fun (r1, n1) (r2, n2) ->
           match compare n2 n1 with 0 -> compare r1 r2 | c -> c) reasons);
    Alcotest.(check int) "decided + incomplete = generated"
      (TestResult.get_count_gen r)
      (TestResult.get_count r + TestResult.get_count_incomplete r)

  let test_empty_reason () =
    let r = run Gen.int (fun _ -> incomplete "") in
    Alcotest.(check (list (pair string int))) "the empty reason is kept"
      ["", TestResult.get_count_gen r] (TestResult.get_incomplete_reasons r)

  let test_other_failures_stay_errors () =
    List.iter (fun msg ->
        match TestResult.get_state (run Gen.int (fun _ -> failwith msg)) with
        | TestResult.Error {exn = Failure m; _} ->
          Alcotest.(check string) "message" msg m
        | _ -> Alcotest.failf "%S must still be an error" msg)
      ["boom"; "TODO"; "TODO:x"]

  let test_shrink_skips_incomplete_candidates () =
    let r = run (Gen.int_bound 1000)
        (fun i -> if i < 300 then incomplete "small" else i < 500) in
    match TestResult.get_state r with
    | TestResult.Failed {instances = [c]} ->
      Alcotest.(check int) "shrunk to the smallest counterexample"
        500 c.TestResult.instance
    | _ -> Alcotest.fail "a genuine counterexample must not become incomplete"

  exception Boom

  let test_shrink_skips_incomplete_candidates_on_error () =
    let r = run (Gen.int_bound 1000)
        (fun i ->
           if i < 300 then incomplete "small"
           else if i >= 500 then raise Boom
           else true) in
    match TestResult.get_state r with
    | TestResult.Error {exn = Boom; instance = {TestResult.instance = 500; _}; _} -> ()
    | _ -> Alcotest.fail "the error must keep its exception and shrink to 500"

  let test_fatal_precondition_still_checked () =
    let r = run ~if_assumptions_fail:(`Fatal, 0.5) Gen.int
        (fun i ->
           if i mod 50 = 0 then incomplete "x";
           assume (i mod 100 = 1);
           true) in
    match TestResult.get_state r with
    | TestResult.Failed_other _ -> ()
    | _ -> Alcotest.fail "`Fatal must still fire when most inputs are discarded"

  let test_check_cell_exn_raises_test_incomplete () =
    List.iter (fun negative ->
        let cell = Test.make_cell ~name:"t" ~count:10 ~negative Gen.int
            (fun _ -> incomplete "later") in
        match Test.check_cell_exn ~rand:(rand ()) cell with
        | () -> Alcotest.fail "an incomplete run must not pass silently"
        | exception Test.Test_incomplete ("t", ["later", _]) -> ()
        | exception e ->
          Alcotest.failf "an incomplete run must raise Test_incomplete, not %s"
            (Printexc.to_string e))
      [false; true]

  let tests = ("Incomplete cases (Incomplete exception)", Alcotest.[
      test_case "all cases incomplete" `Quick test_all_incomplete;
      test_case "several reasons, sorted by count" `Quick test_several_reasons;
      test_case "empty reason" `Quick test_empty_reason;
      test_case "other exceptions stay errors, Failure \"TODO:...\" included" `Quick test_other_failures_stay_errors;
      test_case "shrinking skips incomplete candidates" `Quick test_shrink_skips_incomplete_candidates;
      test_case "shrinking skips incomplete candidates (error)" `Quick test_shrink_skips_incomplete_candidates_on_error;
      test_case "`Fatal precondition check still fires" `Quick test_fatal_precondition_still_checked;
      test_case "check_cell_exn raises Test_incomplete" `Quick test_check_cell_exn_raises_test_incomplete;
    ])
end

module Incomplete_prefix = struct
  open QCheck2

  let rand () = Random.State.make [|0|]

  let run ?incomplete_prefix law =
    Test.check_cell ~rand:(rand ())
      (Test.make_cell ~count:50 ?incomplete_prefix Gen.int law)

  let reasons r = List.map fst (TestResult.get_incomplete_reasons r)

  let test_prefix_on () =
    let r = run ~incomplete_prefix:"TODO:" (fun _ -> failwith "TODO:x") in
    Alcotest.(check bool) "state is Success" true (TestResult.is_success r);
    Alcotest.(check (list (pair string int))) "the reason is the rest of the message"
      ["x", TestResult.get_count_gen r] (TestResult.get_incomplete_reasons r)

  let test_prefix_is_exact () =
    List.iter (fun msg ->
        match TestResult.get_state (run ~incomplete_prefix:"TODO:" (fun _ -> failwith msg)) with
        | TestResult.Error {exn = Failure m; _} -> Alcotest.(check string) "message" msg m
        | _ -> Alcotest.failf "%S must still be an error" msg)
      ["boom"; "TODO"; "todo:x"; " TODO:x"; "a TODO:x"]

  let test_off_by_default () =
    match TestResult.get_state (run (fun _ -> failwith "TODO:x")) with
    | TestResult.Error {exn = Failure m; _} -> Alcotest.(check string) "message" "TODO:x" m
    | _ -> Alcotest.fail "without a prefix, Failure \"TODO:x\" must be an error"

  let test_env_default () =
    Unix.putenv "QCHECK_INCOMPLETE_PREFIX" "TODO:";
    let r = run (fun _ -> failwith "TODO:from env") in
    Unix.putenv "QCHECK_INCOMPLETE_PREFIX" "";
    Alcotest.(check (list string)) "the environment enables the prefix" ["from env"] (reasons r);
    match TestResult.get_state (run (fun _ -> failwith "TODO:x")) with
    | TestResult.Error _ -> ()
    | _ -> Alcotest.fail "an empty QCHECK_INCOMPLETE_PREFIX must disable the adapter"

  let test_argument_wins_over_env () =
    Unix.putenv "QCHECK_INCOMPLETE_PREFIX" "TODO:";
    let other = run ~incomplete_prefix:"FIXME:" (fun _ -> failwith "TODO:x") in
    let off = run ~incomplete_prefix:"" (fun _ -> failwith "TODO:x") in
    Unix.putenv "QCHECK_INCOMPLETE_PREFIX" "";
    (match TestResult.get_state other with
     | TestResult.Error _ -> ()
     | _ -> Alcotest.fail "the explicit prefix must override the environment");
    match TestResult.get_state off with
    | TestResult.Error _ -> ()
    | _ -> Alcotest.fail "an explicit empty prefix must disable the adapter"

  let test_incomplete_always_recognised () =
    let with_prefix = run ~incomplete_prefix:"TODO:" (fun _ -> incomplete "typed") in
    let without = run (fun _ -> incomplete "typed") in
    Alcotest.(check (list string)) "with a prefix" ["typed"] (reasons with_prefix);
    Alcotest.(check (list string)) "without a prefix" ["typed"] (reasons without)

  let test_reraised_failure () =
    let r = run ~incomplete_prefix:"TODO:"
        (fun _ -> try failwith "TODO:re-raised" with exn -> raise exn) in
    Alcotest.(check (list string)) "reason" ["re-raised"] (reasons r)

  let test_message_equal_to_prefix () =
    let r = run ~incomplete_prefix:"TODO:" (fun _ -> failwith "TODO:") in
    Alcotest.(check (list string)) "empty reason" [""] (reasons r)

  let test_shrink_skips_prefixed_candidates () =
    let cell = Test.make_cell ~count:100 ~incomplete_prefix:"TODO:" (Gen.int_bound 1000)
        (fun i -> if i < 300 then failwith "TODO:small" else i < 500) in
    match TestResult.get_state (Test.check_cell ~rand:(rand ()) cell) with
    | TestResult.Failed {instances = [c]} ->
      Alcotest.(check int) "shrunk to the smallest counterexample" 500 c.TestResult.instance
    | _ -> Alcotest.fail "a genuine counterexample must not become incomplete"

  let tests = ("Incomplete cases (incomplete_prefix)", Alcotest.[
      test_case "prefixed Failure is incomplete" `Quick test_prefix_on;
      test_case "the prefix is exact and case-sensitive" `Quick test_prefix_is_exact;
      test_case "off by default" `Quick test_off_by_default;
      test_case "QCHECK_INCOMPLETE_PREFIX sets the default" `Quick test_env_default;
      test_case "the argument wins over the environment" `Quick test_argument_wins_over_env;
      test_case "Incomplete is recognised with or without a prefix" `Quick test_incomplete_always_recognised;
      test_case "a re-raised Failure is still recognised" `Quick test_reraised_failure;
      test_case "message equal to the prefix: empty reason" `Quick test_message_equal_to_prefix;
      test_case "shrinking skips prefixed candidates" `Quick test_shrink_skips_prefixed_candidates;
    ])
end

module Register_incomplete = struct
  (* registrations are global: each one here recognises an exception that
     no other test raises *)
  open QCheck2

  exception Todo of string
  exception Not_a_signal of string
  exception Confusing

  let () =
    register_incomplete (function Todo reason -> Some reason | _ -> None);
    (* a registered function that raises: its exception is the error of the test *)
    register_incomplete (function Confusing -> failwith "broken signal" | _ -> None);
    (* a discarded case is never submitted to the registered functions *)
    register_incomplete (function No_example_found _ -> Some "discard" | _ -> None)

  let rand () = Random.State.make [|0|]

  let run ?incomplete_prefix law =
    Test.check_cell ~rand:(rand ())
      (Test.make_cell ~count:50 ?incomplete_prefix Gen.int law)

  let reasons r = List.map fst (TestResult.get_incomplete_reasons r)

  let test_registered_exception () =
    let r = run (fun _ -> raise (Todo "later")) in
    Alcotest.(check bool) "state is Success" true (TestResult.is_success r);
    Alcotest.(check int) "no case was decided" 0 (TestResult.get_count r);
    Alcotest.(check (list (pair string int))) "the reason comes from the payload"
      ["later", TestResult.get_count_gen r] (TestResult.get_incomplete_reasons r)

  let test_unregistered_exception_stays_error () =
    match TestResult.get_state (run (fun _ -> raise (Not_a_signal "x"))) with
    | TestResult.Error {exn = Not_a_signal "x"; _} -> ()
    | _ -> Alcotest.fail "an exception no registered function recognises must be an error"

  let test_signals_combine () =
    let r = run ~incomplete_prefix:"TODO:" (fun i -> match abs i mod 3 with
        | 0 -> raise (Todo "registered")
        | 1 -> incomplete "typed"
        | _ -> failwith "TODO:prefixed") in
    Alcotest.(check (list string)) "the three signals are recognised in one run"
      ["prefixed"; "registered"; "typed"] (List.sort compare (reasons r))

  let test_raising_function_is_an_error () =
    match TestResult.get_state (run (fun _ -> raise Confusing)) with
    | TestResult.Error {exn = Failure msg; _} when msg = "broken signal" -> ()
    | TestResult.Error {exn; _} ->
      Alcotest.failf "the error must carry the exception of the registered function, not %s"
        (Printexc.to_string exn)
    | _ -> Alcotest.fail "a registered function that raises must make the test an error"

  let test_discards_are_not_submitted () =
    let r = run (fun _ -> raise (No_example_found "x")) in
    Alcotest.(check int) "no incomplete case" 0 (TestResult.get_count_incomplete r);
    Alcotest.(check int) "no case was decided" 0 (TestResult.get_count r)

  let test_shrink_skips_registered_candidates () =
    let cell = Test.make_cell ~count:100 (Gen.int_bound 1000)
        (fun i -> if i < 300 then raise (Todo "small") else i < 500) in
    match TestResult.get_state (Test.check_cell ~rand:(rand ()) cell) with
    | TestResult.Failed {instances = [c]} ->
      Alcotest.(check int) "shrunk to the smallest counterexample" 500 c.TestResult.instance
    | _ -> Alcotest.fail "a genuine counterexample must not become incomplete"

  let tests = ("Incomplete cases (register_incomplete)", Alcotest.[
      test_case "registered exception is incomplete" `Quick test_registered_exception;
      test_case "unregistered exception stays an error" `Quick test_unregistered_exception_stays_error;
      test_case "the three signals combine in one run" `Quick test_signals_combine;
      test_case "a function that raises is an error of the test" `Quick test_raising_function_is_an_error;
      test_case "discards are not submitted" `Quick test_discards_are_not_submitted;
      test_case "shrinking skips registered candidates" `Quick test_shrink_skips_registered_candidates;
    ])
end

module Backtrace = struct
  (* The signals of incompleteness are recognised in the [when] guard of the
     handler that runs the property, so an exception that is none of them is
     not caught and re-raised: its backtrace keeps the frame of the raise. *)
  open QCheck2

  exception Boom

  let boom_line = __LINE__ and boom () = raise Boom

  let rand () = Random.State.make [|0|]

  (* [String] is the test module of that name above: hence [Stdlib.String] *)
  let contains s sub =
    let n = Stdlib.String.length s and m = Stdlib.String.length sub in
    let rec go i = i + m <= n && (Stdlib.String.sub s i m = sub || go (i + 1)) in
    go 0

  let test_error_keeps_the_raise_site () =
    Printexc.record_backtrace true;
    let r = Test.check_cell ~rand:(rand ())
        (Test.make_cell ~count:10 Gen.int (fun _ -> boom ())) in
    match TestResult.get_state r with
    | TestResult.Error {exn = Boom; backtrace; _} ->
      let first = match Stdlib.String.split_on_char '\n' backtrace with
        | first :: _ -> first | [] -> "" in
      Alcotest.(check bool)
        (Printf.sprintf "the first frame is the raise in %S, line %d:\n%s" __FILE__ boom_line backtrace)
        true (contains first "Raised at"
              && contains first (Printf.sprintf "%S" __FILE__)
              && contains first (Printf.sprintf "line %d," boom_line));
      (* natively, the re-raise the compiler inserts when the guard is false
         leaves no entry; in bytecode it does, so this is the native check *)
      (match Sys.backend_type with
       | Sys.Native ->
         Alcotest.(check bool) ("no frame of qcheck re-raising it:\n" ^ backtrace)
           false (contains backtrace "Re-raised at")
       | Sys.Bytecode | Sys.Other _ -> ())
    | _ -> Alcotest.fail "Boom must be an error"

  let tests = ("Incomplete cases (backtrace of the other exceptions)", Alcotest.[
      test_case "an error keeps the frame of its raise" `Quick test_error_keeps_the_raise_site;
    ])
end

let () =
  Alcotest.run "QCheck2"
    [
      Shrink.tests;
      Gen.tests;
      TestCount.tests;
      TestLongFactor.tests;
      String.tests;
      Check_exn.tests;
      Incomplete.tests;
      Incomplete_prefix.tests;
      Register_incomplete.tests;
      Backtrace.tests;
      Fun_todo_tests.tests;
    ]
