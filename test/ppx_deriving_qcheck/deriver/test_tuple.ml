open QCheck

type a = char [@gen QCheck.Gen.pure 'a']
[@@deriving qcheck]

type b = char [@gen QCheck.Gen.pure 'b']
[@@deriving qcheck]

type c = char [@gen QCheck.Gen.pure 'c']
[@@deriving qcheck]

type d = char [@gen QCheck.Gen.pure 'd']
[@@deriving qcheck]

type e = char [@gen QCheck.Gen.pure 'e']
[@@deriving qcheck]

type f = char [@gen QCheck.Gen.pure 'f']
[@@deriving qcheck]

type g = char [@gen QCheck.Gen.pure 'g']
[@@deriving qcheck]

type h = char [@gen QCheck.Gen.pure 'h']
[@@deriving qcheck]

type i = char [@gen QCheck.Gen.pure 'i']
[@@deriving qcheck]

type tup2 = a * b
[@@deriving qcheck]

type tup3 = a * b * c
[@@deriving qcheck]

type tup4 = a * b * c * d
[@@deriving qcheck]

type tup5 = a * b * c * d * e
[@@deriving qcheck]

type tup6 = a * b * c * d * e * f
[@@deriving qcheck]

type tup7 = a * b * c * d * e * f * g
[@@deriving qcheck]

type tup8 = a * b * c * d * e * f * g * h
[@@deriving qcheck]

let test_tup2 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b'): x = ('a', 'b')"
    (make gen_tup2)
    (fun x -> x = ('a', 'b'))

let test_tup3 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c'): x = ('a', 'b', 'c')"
    (make gen_tup3)
    (fun x -> x = ('a', 'b', 'c'))

let test_tup4 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c', 'd'): x = ('a', 'b', 'c', 'd')"
    (make gen_tup4)
    (fun x -> x = ('a', 'b', 'c', 'd'))

let test_tup5 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c', 'd', 'e'): x = ('a', 'b', 'c', 'd', 'e')"
    (make gen_tup5)
    (fun x -> x = ('a', 'b', 'c', 'd', 'e'))

let test_tup6 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c', 'd', 'e', 'f'): x = ('a', 'b', 'c', 'd', 'e', 'f')"
    (make gen_tup6)
    (fun x -> x = ('a', 'b', 'c', 'd', 'e', 'f'))

let test_tup7 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c', 'd', 'e', 'f', 'g'): x = ('a', 'b', 'c', 'd', 'e', 'f', 'g')"
    (make gen_tup7)
    (fun x -> x = ('a', 'b', 'c', 'd', 'e', 'f', 'g'))

let test_tup8 =
  Test.make ~count:10
    ~name:"forall x in ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'): x = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')"
    (make gen_tup8)
    (fun x -> x = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))

let tests = [
    test_tup2;
    test_tup3;
    test_tup4;
    test_tup5;
    test_tup6;
    test_tup7;
    test_tup8;
  ]

let tests = List.map (QCheck_alcotest.to_alcotest) tests

(** {2. Execute tests} *)
let () = Alcotest.run "Test_Tuple" [("Tuple", tests)]
