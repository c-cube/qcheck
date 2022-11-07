open Ppxlib

(** This module contains all generators from QCheck used to
    derive a type declaration *)

(** {2. Version} *)

type version = [`QCheck | `QCheck2]

let to_module : version -> string = function
  | `QCheck -> "QCheck"
  | `QCheck2 -> "QCheck2"

let with_prefix loc version prefix x =
  let (module A) = Ast_builder.make loc in
  A.Located.mk @@ Ldot (Ldot (Lident (to_module version), prefix), x)
  |> A.pexp_ident

let with_prefix_gen loc version x = with_prefix loc version "Gen" x

let with_prefix_obs loc version x = with_prefix loc version "Observable" x

let apply1 loc f a = [%expr [%e f] [%e a]]

let apply2 loc f a b = [%expr [%e f] [%e a] [%e b]]

let apply3 loc f a b c  = [%expr [%e f] [%e a] [%e b] [%e c]]

let apply4 loc f a b c d = [%expr [%e f] [%e a] [%e b] [%e c] [%e d]]

(** {2. Type} *)

let ty version = Ldot (Ldot (Lident (to_module version), "Gen"), "t")

(** {2. Primitive generators} *)

let unit loc version = with_prefix_gen loc version "unit"

let int loc version = with_prefix_gen loc version "int"

let string loc version = with_prefix_gen loc version "string"

let char loc version = with_prefix_gen loc version "char"

let bool loc version = with_prefix_gen loc version "bool"

let float loc version = with_prefix_gen loc version "float"

let int32 loc version = with_prefix_gen loc version "ui32"

let int64 loc version = with_prefix_gen loc version "ui64"

let option ~loc ~version e =
  let gen = with_prefix_gen loc version "option" in
  apply1 loc gen e

let list ~loc ~version e =
  let gen = with_prefix_gen loc version "list" in
  apply1 loc gen e

let array ~loc ~version e =
  let gen = with_prefix_gen loc version "array" in
  apply1 loc gen e

(** {2. Generator combinators} *)

let pure ~loc ~version e =
  let gen = with_prefix_gen loc version "pure" in
  apply1 loc gen e

let frequency ~loc ~version l =
  match l with
  | [%expr [([%e? _], [%e? x])]] -> x
  | _ ->
     let gen = with_prefix_gen loc version "frequency" in
     apply1 loc gen l

let map ~loc ~version pat expr gen =
  let f = with_prefix_gen loc version "map" in
  apply2 loc f [%expr fun [%p pat] -> [%e expr]] gen

let pair ~loc ~version a b =
  let gen = with_prefix_gen loc version "pair" in
  apply2 loc gen a b

let triple ~loc ~version a b c =
  let gen = with_prefix_gen loc version "triple" in
  apply3 loc gen a b c

let quad ~loc ~version a b c d =
  let gen = with_prefix_gen loc version "quad" in
  apply4 loc gen a b c d

let sized ~loc ~version e =
  let gen = with_prefix_gen loc version "sized" in
  apply1 loc gen e

let fix ~loc ~version e =
  let gen = with_prefix_gen loc version "fix" in
  apply1 loc gen e

(** Observable generators *)
module Observable = struct
  (** {2. Primitive generators} *)
  let unit loc version = with_prefix_obs loc version "unit"

  let int loc version = with_prefix_obs loc version "int"

  let string loc version = with_prefix_obs loc version "string"

  let char loc version = with_prefix_obs loc version "char"

  let bool loc version = with_prefix_obs loc version "bool"

  let float loc version = with_prefix_obs loc version "float"

  let int32 loc version = with_prefix_obs loc version "int32"

  let int64 loc version = with_prefix_obs loc version "int64"

  let option ~loc ~version e =
    let obs = with_prefix_obs loc version "option" in
    apply1 loc obs e

  let list ~loc ~version e =
    let obs = with_prefix_obs loc version "list" in
    apply1 loc obs e

  let array ~loc ~version e =
    let obs = with_prefix_obs loc version "array" in
    apply1 loc obs e

  (** {2. Observable combinators} *)
  let pair ~loc ~version a b =
    let obs = with_prefix_obs loc version "pair" in
    apply2 loc obs a b

  let triple ~loc ~version a b c =
    let obs = with_prefix_obs loc version "triple" in
    apply3 loc obs a b c

  let quad ~loc ~version a b c d =
    let obs = with_prefix_obs loc version "quad" in
    apply4 loc obs a b c d

  let fun_nary ~loc ~version left right gen =
    match version with
    | `QCheck ->
       let arb = [%expr QCheck.make [%e gen]] in
       [%expr QCheck.fun_nary QCheck.Tuple.([%e left] @-> [%e right]) [%e arb] |> QCheck.gen]
    | `QCheck2 ->
       [%expr QCheck2.fun_nary QCheck2.Tuple.([%e left] @-> [%e right]) [%e gen]]
end

module Make (Version : sig val version : version end) = struct
  let version = Version.version
  let ty = ty version
  let unit loc = unit loc version
  let int loc = int loc version
  let string loc = string loc version
  let char loc = char loc version
  let bool loc = bool loc version
  let float loc = float loc version
  let int32 loc = int32 loc version
  let int64 loc = int64 loc version
  let option ~loc = option ~loc ~version
  let list ~loc = list ~loc ~version
  let array ~loc = array ~loc ~version
  let pure ~loc x = pure ~loc ~version x
  let frequency ~loc l = frequency ~loc ~version l
  let map ~loc pat expr gen = map ~loc ~version pat expr gen
  let pair ~loc a b = pair ~loc ~version a b
  let triple ~loc a b c = triple ~loc ~version a b c
  let quad ~loc a b c d = quad ~loc ~version a b c d
  let sized ~loc e = sized ~loc ~version e
  let fix ~loc e = fix ~loc ~version e
  module Observable = struct
    let unit loc = Observable.unit loc version
    let int loc = Observable.int loc version
    let string loc = Observable.string loc version
    let char loc = Observable.char loc version
    let bool loc = Observable.bool loc version
    let float loc = Observable.float loc version
    let int32 loc = Observable.int32 loc version
    let int64 loc = Observable.int64 loc version
    let option ~loc e = Observable.option ~loc ~version e
    let list ~loc e = Observable.list ~loc ~version e
    let array ~loc e = Observable.array ~loc ~version e
    let pair ~loc a b = Observable.pair ~loc ~version a b
    let triple ~loc a b c = Observable.triple ~loc ~version a b c
    let quad ~loc a b c d = Observable.quad ~loc ~version a b c d
    let fun_nary ~loc left right gen = Observable.fun_nary ~loc ~version left right gen
  end
end

module QCheck = Make (struct let version = `QCheck end)
module QCheck2 = Make (struct let version = `QCheck2 end)
module type S = module type of QCheck

let make version = (module Make (struct let version = version end) : S)
