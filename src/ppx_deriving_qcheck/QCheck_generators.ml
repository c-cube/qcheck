open Ppxlib

(** This module contains all generators from QCheck used to
    derive a type declaration *)

(** {2. Type} *)

let ty = function
  | `QCheck -> Ldot (Ldot (Lident "QCheck", "Gen"), "t")
  | `QCheck2 -> Ldot (Ldot (Lident "QCheck2", "Gen"), "t")

(** {2. Primitive generators} *)

let unit loc = function
  | `QCheck -> [%expr QCheck.Gen.unit]
  | `QCheck2 -> [%expr QCheck2.Gen.unit]

let int loc = function
  | `QCheck -> [%expr QCheck.Gen.int]
  | `QCheck2 -> [%expr QCheck2.Gen.int]

let string loc = function
  | `QCheck -> [%expr QCheck.Gen.string]
  | `QCheck2 -> [%expr QCheck2.Gen.string]

let char loc = function
  | `QCheck -> [%expr QCheck.Gen.char]
  | `QCheck2 -> [%expr QCheck2.Gen.char]

let bool loc = function
  | `QCheck -> [%expr QCheck.Gen.bool]
  | `QCheck2 -> [%expr QCheck2.Gen.bool]

let float loc = function
  | `QCheck -> [%expr QCheck.Gen.float]
  | `QCheck2 -> [%expr QCheck2.Gen.float]

let int32 loc = function
  | `QCheck -> [%expr QCheck.Gen.ui32]
  | `QCheck2 -> [%expr QCheck2.Gen.ui32]

let int64 loc = function
  | `QCheck -> [%expr QCheck.Gen.ui64]
  | `QCheck2 -> [%expr QCheck2.Gen.ui64]

let option ~loc ~version e =
  match version with
  | `QCheck -> [%expr QCheck.Gen.option [%e e]]
  | `QCheck2 -> [%expr QCheck2.Gen.opt [%e e]]

let list ~loc ~version e =
  match version with
  | `QCheck -> [%expr QCheck.Gen.list [%e e]]
  | `QCheck2 -> [%expr QCheck2.Gen.list [%e e]]

let array ~loc ~version e =
  match version with
  | `QCheck -> [%expr QCheck.Gen.array [%e e]]
  | `QCheck2 -> [%expr QCheck2.Gen.array [%e e]]

(** {2. Generator combinators} *)

let pure ~loc ~version x =
  match version with
  | `QCheck -> [%expr QCheck.Gen.pure [%e x]]
  | `QCheck2 -> [%expr QCheck2.Gen.pure [%e x]]

let frequency ~loc ~version l =
  match l with
  | [%expr [([%e? _], [%e? x])]] -> x
  | _ ->
     (match version with
      | `QCheck -> [%expr QCheck.Gen.frequency [%e l]]
      | `QCheck2 -> [%expr QCheck2.Gen.frequency [%e l]])

let map ~loc ~version pat expr gen =
  match version with
  | `QCheck -> [%expr QCheck.Gen.map (fun [%p pat] -> [%e expr]) [%e gen]]
  | `QCheck2 -> [%expr QCheck2.Gen.map (fun [%p pat] -> [%e expr]) [%e gen]]

let pair ~loc ~version a b =
  match version with
  | `QCheck -> [%expr QCheck.Gen.pair [%e a] [%e b]]
  | `QCheck2 -> [%expr QCheck2.Gen.pair [%e a] [%e b]]

let triple ~loc ~version a b c =
  match version with
  | `QCheck -> [%expr QCheck.Gen.triple [%e a] [%e b] [%e c]]
  | `QCheck2 -> [%expr QCheck2.Gen.triple [%e a] [%e b] [%e c]]

let quad ~loc ~version a b c d =
  match version with
  | `QCheck -> [%expr QCheck.Gen.quad [%e a] [%e b] [%e c] [%e d]]
  | `QCheck2 -> [%expr QCheck2.Gen.quad [%e a] [%e b] [%e c] [%e d]]

let sized ~loc ~version e =
  match version with
  | `QCheck -> [%expr QCheck.Gen.sized @@ [%e e]]
  | `QCheck2 -> [%expr QCheck2.Gen.sized @@ [%e e]]

let fix ~loc ~version e =
  match version with
  | `QCheck -> [%expr QCheck.Gen.fix [%e e]]
  | `QCheck2 -> [%expr QCheck2.Gen.fix [%e e]]

(** Observable generators *)
module Observable = struct
  (** {2. Primitive generators} *)
  let unit loc = function
    | `QCheck ->  [%expr QCheck.Observable.unit]
    | `QCheck2 ->  [%expr QCheck2.Observable.unit]

  let int loc = function
    | `QCheck ->  [%expr QCheck.Observable.int]
    | `QCheck2 ->  [%expr QCheck2.Observable.int]

  let string loc = function
    | `QCheck ->  [%expr QCheck.Observable.string]
    | `QCheck2 ->  [%expr QCheck2.Observable.string]

  let char loc = function
    | `QCheck ->  [%expr QCheck.Observable.char]
    | `QCheck2 ->  [%expr QCheck2.Observable.char]

  let bool loc = function
    | `QCheck ->  [%expr QCheck.Observable.bool]
    | `QCheck2 ->  [%expr QCheck2.Observable.bool]

  let float loc = function
    | `QCheck ->  [%expr QCheck.Observable.float]
    | `QCheck2 ->  [%expr QCheck2.Observable.float]

  let int32 loc = function
    | `QCheck ->  [%expr QCheck.Observable.int32]
    | `QCheck2 ->  [%expr QCheck2.Observable.int32]

  let int64 loc = function
    | `QCheck ->  [%expr QCheck.Observable.int64]
    | `QCheck2 ->  [%expr QCheck2.Observable.int64]

  let option ~loc ~version e =
    match version with
    | `QCheck -> [%expr QCheck.Observable.option [%e e]]
    | `QCheck2 -> [%expr QCheck2.Observable.option [%e e]]

  let list ~loc ~version e =
    match version with
    | `QCheck ->  [%expr QCheck.Observable.list [%e e]]
    | `QCheck2 ->  [%expr QCheck2.Observable.list [%e e]]

  let array ~loc ~version e =
    match version with
    | `QCheck ->  [%expr QCheck.Observable.array [%e e]]
    | `QCheck2 ->  [%expr QCheck2.Observable.array [%e e]]

  (** {2. Observable combinators} *)
  let pair ~loc ~version a b =
    match version with
    | `QCheck -> [%expr QCheck.Observable.pair [%e a] [%e b]]
    | `QCheck2 -> [%expr QCheck2.Observable.pair [%e a] [%e b]]

  let triple ~loc ~version a b c =
    match version with
    | `QCheck -> [%expr QCheck.Observable.triple [%e a] [%e b] [%e c]]
    | `QCheck2 -> [%expr QCheck2.Observable.triple [%e a] [%e b] [%e c]]

  let quad ~loc ~version a b c d =
    match version with
    | `QCheck -> [%expr QCheck.Observable.quad [%e a] [%e b] [%e c] [%e d]]
    | `QCheck2 -> [%expr QCheck2.Observable.quad [%e a] [%e b] [%e c] [%e d]]

  let fun_nary ~loc ~version left right gen =
    match version with
    | `QCheck ->
       let arb = [%expr QCheck.make [%e gen]] in
       [%expr QCheck.fun_nary QCheck.Tuple.([%e left] @-> [%e right]) [%e arb] |> QCheck.gen]
    | `QCheck2 ->
       [%expr QCheck2.fun_nary QCheck2.Tuple.([%e left] @-> [%e right]) [%e gen]]
end

module Make (Version : sig val version : [`QCheck | `QCheck2] end) = struct
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
