open Ppxlib

(** This module contains all generators from QCheck used to
    derive a type declaration *)

(** {2. Type} *)

let ty = "QCheck.Gen.t"

(** {2. Primitive generators} *)

let unit loc = [%expr QCheck.Gen.unit]

let int loc = [%expr QCheck.Gen.int]

let string loc = [%expr QCheck.Gen.string]

let char loc = [%expr QCheck.Gen.char]

let bool loc = [%expr QCheck.Gen.bool]

let float loc = [%expr QCheck.Gen.float]

let int32 loc = [%expr QCheck.Gen.ui32]

let int64 loc = [%expr QCheck.Gen.ui64]

let option ~loc e = [%expr QCheck.Gen.opt [%e e]]

let list ~loc e = [%expr QCheck.Gen.list [%e e]]

let array ~loc e = [%expr QCheck.Gen.array [%e e]]

(** {2. Generator combinators} *)

let pure ~loc x = [%expr QCheck.Gen.pure [%e x]] 

let frequency ~loc l =
  match l with
  | [%expr [([%e? _], [%e? x])]] -> x
  | _ ->
     [%expr QCheck.Gen.frequency [%e l]]

let map ~loc pat expr gen =
  [%expr QCheck.Gen.map (fun [%p pat] -> [%e expr]) [%e gen]]

let pair ~loc a b =
  [%expr QCheck.Gen.pair [%e a] [%e b]]

let triple ~loc a b c =
  [%expr QCheck.Gen.triple [%e a] [%e b] [%e c]]

let quad ~loc a b c d=
  [%expr QCheck.Gen.quad [%e a] [%e b] [%e c] [%e d]]

let sized ~loc e =
  [%expr QCheck.Gen.sized @@ [%e e]]

let fix ~loc e =
  [%expr QCheck.Gen.fix [%e e]]

(** Observable generators *)
module Observable = struct
  (** {2. Primitive generators} *)
  let unit loc = [%expr QCheck.Observable.unit]

  let int loc = [%expr QCheck.Observable.int]

  let string loc = [%expr QCheck.Observable.string]

  let char loc = [%expr QCheck.Observable.char]

  let bool loc = [%expr QCheck.Observable.bool]

  let float loc = [%expr QCheck.Observable.float]

  let int32 loc = [%expr QCheck.Observable.int32]

  let int64 loc = [%expr QCheck.Observable.int64]

  let option ~loc e = [%expr QCheck.Observable.option [%e e]]

  let list ~loc e = [%expr QCheck.Observable.list [%e e]]

  let array ~loc e = [%expr QCheck.Observable.array [%e e]]

  (** {2. Observable combinators} *)
  let pair ~loc a b =
  [%expr QCheck.Observable.pair [%e a] [%e b]]

  let triple ~loc a b c =
    [%expr QCheck.Observable.triple [%e a] [%e b] [%e c]]

  let quad ~loc a b c d=
    [%expr QCheck.Observable.quad [%e a] [%e b] [%e c] [%e d]]
end
