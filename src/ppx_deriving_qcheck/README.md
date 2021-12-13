# ppx_deriving_qcheck

## Generator
Derive `QCheck.Gen.t` from a type declaration

```ocaml
type tree = Leaf of int | Node of tree * tree
[@@deriving qcheck]

let rec rev tree = match tree with
| Leaf _ -> tree
| Node (left, right) -> Node (rev right, rev left)

let test =
  QCheck.Test.make
    ~name:"tree -> rev (rev tree) = tree"
	(QCheck.make gen_tree)
	(fun tree -> rev (rev tree) = tree)
```

For `type tree` we derive two generators:
- `val gen_tree : tree Gen.t` and
- `val gen_tree_sized : int -> tree Gen.t`

For non-recursive types the latter is however not derived.

For types with the name `t` (i.e. `type t = ...`) which is a common idiom in OCaml code,
the deriver omits the name from the derived generators,
thus producing `val gen : t Gen.t` and optionally `val gen_sized : int -> t Gen.t`.

### Overwrite generator
If you wan't to specify your own `generator` for any type you can
add an attribute to the type:

```ocaml
type t = (int : [@gen QCheck.Gen.(0 -- 10)])
[@@deriving qcheck]

(* produces ==> *)

let gen : t QCheck.Gen.t = QCheck.Gen.(0 -- 10)
```

This attribute has 2 advantages:
* Use your own generator for a specific type (see above)
* There is no generator available for the type
  ```ocaml
  type my_foo =
  | Foo of my_other_type
  | Bar of bool
  [@@deriving qcheck]
  ^^^^^^^^^^^^^^^^
  Error: Unbound value gen_my_other_type
  
  (* Possible fix *)
  let gen_my_other_type = (* add your implementation here *)
  
  type my_foo =
  | Foo of my_other_type [@gen gen_my_other_type]
  | Bar of bool
  [@@deriving qcheck]
  ```

## How to use

Add to your OCaml libraries with dune
```ocaml
...
(preprocess (pps ppx_deriving_qcheck)))
...
```

## Supported types

### Primitive types

* Unit
```ocaml
type t = unit [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.unit
```

* Bool
```ocaml
type t = bool [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.bool
```

* Integer
```ocaml
type t = int [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.int
```

* Float
```ocaml
type t = float [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.float
```

* String
```ocaml
type t = string [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.string
```

* Char
```ocaml
type t = char [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.char
```

* Option
```ocaml
type 'a t = 'a option [@@deriving qcheck]

(* ==> *)

let gen gen_a = QCheck.Gen.option gen_a
```

* List
```ocaml
type 'a t = 'a list [@@deriving qcheck]

(* ==> *)

let gen gen_a = QCheck.Gen.list gen_a
```

* Array
```ocaml
type 'a t = 'a array [@@deriving qcheck]

(* ==> *)

let gen gen_a = QCheck.Gen.array gen_a
```

### Tuples of size `n`

* n = 2
```ocaml
type t = int * int [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.pair QCheck.Gen.int QCheck.Gen.int
```

* n = 3
```ocaml
type t = int * int * int [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.triple QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int
```

* n = 4
```ocaml
type t = int * int * int * int [@@deriving qcheck]

(* ==> *)

let gen = QCheck.Gen.quad QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int
```

* n > 4, tuples are split between pairs, for instance n = 8
```ocaml
type t = int * int * int * int * int * int * int * int [@@deriving qcheck]

(* ==> *)

let gen =
  QCheck.Gen.pair
    (QCheck.Gen.quad QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int)
    (QCheck.Gen.quad QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int QCheck.Gen.int)
```

## Records
```ocaml
type service = {
	service_name : string;
	port : int;
	protocol : string;
} [@@deriving qcheck]

(* ==> *)

let gen_service =
  QCheck.Gen.map
    (fun (gen0, gen1, gen2) ->
      { service_name = gen0; port = gen1; protocol = gen2 })
    (QCheck.Gen.triple QCheck.Gen.string QCheck.Gen.int QCheck.Gen.string)
```

## Variants
* Variants
```ocaml
type color = Red | Blue | Green
[@@deriving qcheck]

(* ==> *)

let gen_color =
  QCheck.Gen.frequency
    [(1, (QCheck.Gen.pure Red));
     (1, (QCheck.Gen.pure Blue));
     (1, (QCheck.Gen.pure Green))]
```

* Polymorphic variants
```ocaml
type color = [ `Red | `Blue | `Green ]
[@@deriving qcheck]

(* ==> *)

let gen_color =
  (QCheck.Gen.frequency
    [(1, (QCheck.Gen.pure `Red));
     (1, (QCheck.Gen.pure `Blue));
     (1, (QCheck.Gen.pure `Green))] : color QCheck.Gen.t)
```

## Recursive variants
* Recursive variants
```ocaml
type tree = Leaf of int | Node of tree * tree
[@@deriving qcheck]

(* ==> *)

let rec gen_tree_sized n =
  match n with
  | 0 -> QCheck.Gen.map (fun gen0 -> Leaf gen0) QCheck.Gen.int
  | n ->
    QCheck.Gen.frequency
      [(1, (QCheck.Gen.map (fun gen0 -> Leaf gen0) QCheck.Gen.int));
       (1,
		   (QCheck.Gen.map (fun (gen0, gen1) -> Node (gen0, gen1))
             (QCheck.Gen.pair (self (n / 2)) (self (n / 2)))))]))

let gen_tree = QCheck.Gen.sized @@ gen_tree_sized
```

* Recursive polymorphic variants
```ocaml
type tree = [ `Leaf of int | `Node of tree * tree ]
[@@deriving qcheck]

(* ==> *)

let gen_tree =
  (QCheck.Gen.sized @@ QCheck.Gen.fix (fun self -> function
  | 0 ->
    QCheck.Gen.frequency [
	  ( 1, QCheck.Gen.map (fun gen0 -> `Leaf gen0) QCheck.Gen.int);
    ]
  | n ->
    QCheck.Gen.frequency [
      ( 1, QCheck.Gen.map (fun gen0 -> `Leaf gen0) QCheck.Gen.int);
      ( 1,
           QCheck.Gen.map (fun gen0 -> `Node gen0)
             (QCheck.Gen.map
               (fun (gen0, gen1) -> (gen0, gen1))
                 (QCheck.Gen.pair (self (n / 2)) (self (n / 2)))))
                      ])
            : tree QCheck.Gen.t)
```

## Mutual recursive types
```ocaml
type tree = Node of (int * forest)
and forest = Nil | Cons of (tree * forest)
[@@deriving qcheck]

(* ==> *)

let rec gen_tree () =
  QCheck.Gen.frequency
    [(1,
      (QCheck.Gen.map (fun gen0 -> Node gen0)
        (QCheck.Gen.map (fun (gen0, gen1) -> (gen0, gen1))
          (QCheck.Gen.pair QCheck.Gen.int (gen_forest ())))))]

and gen_forest () =
  QCheck.Gen.sized @@
    (QCheck.Gen.fix
      (fun self -> function
        | 0 -> QCheck.Gen.frequency [(1, (QCheck.Gen.pure Nil))]
        | n ->
          QCheck.Gen.frequency
            [(1, (QCheck.Gen.pure Nil));
             (1,
                 (QCheck.Gen.map (fun gen0 -> Cons gen0)
                   (QCheck.Gen.map (fun (gen0, gen1) -> (gen0, gen1))
                     (QCheck.Gen.pair (gen_tree ()) (self (n / 2))))))]))

let gen_tree = gen_tree ()

let gen_forest = gen_forest ()
```

## Unsupported types

### GADT
Deriving a GADT currently produces an ill-typed generator.

### Let us know
If you encounter a unsupported type (that should be), please let us know by creating
an issue.
