open QCheck
open Helpers

type env = {
    rec_types : string list;
    curr_types : string list;
    curr_type : string
  }
[@@deriving qcheck]

let pp_env fmt {rec_types; curr_types; curr_type} =
  let open Format in
  fprintf fmt {|{
  rec_types = [%a];
  curr_types = [%a];
  curr_type = [%s];
}|}
    (pp_print_list pp_print_string) rec_types
    (pp_print_list pp_print_string) curr_types
    curr_type

let eq_env = Alcotest.of_pp pp_env

let gen_env_ref =
  let open Gen in
  map3 (fun rec_types curr_types curr_type ->
      { rec_types; curr_types; curr_type })
    (list string) (list string) string

let test_env () =
  test_compare ~msg:"gen_env ref <=> deriving env"
  ~eq:eq_env gen_env_ref gen_env

type color = Color of { red : float; green : float; blue : float }
[@@deriving qcheck]

let pp_color fmt (Color {red; green; blue}) =
  let open Format in
  fprintf fmt {|Color {
  red = %a;
  green = %a;
  blue = %a;
}|}
    pp_print_float red
    pp_print_float green
    pp_print_float blue

let eq_color = Alcotest.of_pp pp_color

let gen_color_ref =
  let open Gen in
  map3 (fun red green blue -> Color {red; green; blue}) float float float

let test_color () =
  test_compare ~msg:"gen_color ref <=> deriving color"
  ~eq:eq_color gen_color_ref gen_color

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Record"
           [("Record",
             Alcotest.[
                 test_case "test_env" `Quick test_env;
                 test_case "test_color" `Quick test_color;
           ])]
