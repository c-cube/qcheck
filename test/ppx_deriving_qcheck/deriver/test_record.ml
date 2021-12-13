type t = {
    rec_types : string list;
    curr_types : string list;
    curr_type : string
  }
[@@deriving qcheck]

type color = Color of { red : float; green : float; blue : float }
[@@deriving qcheck]

(* TODO: use these types to test generated values inside records.
   For now, having these ensure the compilation *)
