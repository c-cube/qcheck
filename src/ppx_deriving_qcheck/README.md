# ppx_deriving_qcheck

## Generator
Derive `QCheck.Gen.t` from a type declaration

```ocaml
type tree = Leaf of int | Node of tree * tree
[@@deriving qcheck]
```

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
