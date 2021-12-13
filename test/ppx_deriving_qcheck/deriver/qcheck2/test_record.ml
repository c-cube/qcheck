type t = {
    rec_types : string list;
    curr_types : string list;
    curr_type : string
  }
[@@deriving qcheck2]

type color = Color of { red : float; green : float; blue : float }
[@@deriving qcheck2]

(* TODO: use these types to test generated values inside records.
   For now, having these ensure the compilation *)
