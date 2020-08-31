
let shims_let_op_pre_408 =
  "
   module type S = sig type 'a t_let end
   module Make(X:sig type 'a t end) = struct type 'a t_let = 'a X.t end
"
let shims_let_op_post_408 =
  "
 module type S = sig
   type 'a t_let
   val (let+) : 'a t_let -> ('a -> 'b) -> 'b t_let
   val (and+) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
   val (let*) : 'a t_let -> ('a -> 'b t_let) -> 'b t_let
   val (and*) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
 end
 module Make(X:sig
  type 'a t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val monoid_product : 'a t -> 'b t -> ('a * 'b) t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end) : S with type 'a t_let = 'a X.t = struct
    type 'a t_let = 'a X.t
    let (let+) = X.(>|=)
    let (and+) = X.monoid_product
    let (let*) = X.(>>=)
    let (and*) = X.monoid_product
end[@@inline]

"

let split_on c s =
  let l = ref [] in
  let i = ref 0 in
  while !i < String.length s do
    let j = try String.index_from s !i c with Not_found -> String.length s in
    l := String.sub s !i (j- !i) :: !l;
    i := j+1;
  done;
  List.rev !l

let () =
  let maj, min = match split_on '.' Sys.ocaml_version with
    | m1 :: m2 :: _ -> int_of_string m1, int_of_string m2
    | _ -> failwith "cannot parse ocaml version"
  in
  if (maj,min) >= (4,8) then (
    print_endline shims_let_op_post_408
  ) else (
    print_endline shims_let_op_pre_408
  )
