type tup2 = int * int
[@@deriving qcheck]

type tup3 = int * int * int
[@@deriving qcheck]

type tup4 = int * int * int * int
[@@deriving qcheck]

type tup5 = int * int * int * int * int
[@@deriving qcheck]

type tup6 = int * int * int * int * int * int
[@@deriving qcheck]

type tup7 = int * int * int * int * int * int * int
[@@deriving qcheck]

type tup8 = int * int * int * int * int * int * int * int
[@@deriving qcheck]

(* TODO: use these types to test generated values inside tuples.
   For now, having these ensure the compilation *)
