type tup2 = int * int
[@@deriving qcheck2]

type tup3 = int * int * int
[@@deriving qcheck2]

type tup4 = int * int * int * int
[@@deriving qcheck2]

type tup5 = int * int * int * int * int
[@@deriving qcheck2]

type tup6 = int * int * int * int * int * int
[@@deriving qcheck2]

type tup7 = int * int * int * int * int * int * int
[@@deriving qcheck2]

type tup8 = int * int * int * int * int * int * int * int
[@@deriving qcheck2]

(* TODO: use these types to test generated values inside tuples.
   For now, having these ensure the compilation *)
