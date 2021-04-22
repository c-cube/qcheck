(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

(** The module type that both {!QCheck} and {!QCheck2} must match. *)
module type QCHECK = sig
  type 'a arbitrary
  type 'a stat = string * ('a -> int)
  val get_print : 'a arbitrary -> ('a -> string) option
  module TestResult : sig
    type 'a t
    type 'a counter_ex = {
      instance: 'a;
      shrink_steps: int;
      msg_l: string list;
    }
    type 'a failed_state = 'a counter_ex list
    type 'a state =
      | Success
      | Failed of {
          instances: 'a failed_state;
        }
      | Failed_other of {msg: string}
      | Error of {
          instance: 'a counter_ex;
          exn: exn;
          backtrace: string;
        }
    val get_count : _ t -> int
    val get_count_gen : _ t -> int
    val get_state : 'a t -> 'a state
    val is_success : _ t -> bool
    val collect : _ t -> (string,int) Hashtbl.t option
    val warnings : _ t -> string list
    val stats : 'a t -> ('a stat * (int,int) Hashtbl.t) list
  end
  module Test : sig
    type 'a cell
    type t = Test : 'a cell -> t
    type res =
      | Success
      | Failure
      | FalseAssumption
      | Error of exn * string
    type 'a event =
      | Generating
      | Collecting of 'a
      | Testing of 'a
      | Shrunk of int * 'a
      | Shrinking of int * int * 'a
    type 'a handler = string -> 'a cell -> 'a event -> unit
    type 'a step = string -> 'a cell -> 'a -> res -> unit
    type 'a callback = string -> 'a cell -> 'a TestResult.t -> unit
    val get_arbitrary : 'a cell -> 'a arbitrary
    val get_count : _ cell -> int
    val get_long_factor : _ cell -> int
    val get_name : _ cell -> string
    val print_collect : (string, int) Hashtbl.t -> string
    val print_fail : 'a arbitrary -> string -> 'a TestResult.failed_state -> string
    val print_fail_other : string -> msg:string -> string
    val print_error : ?st:string -> 'a arbitrary -> string -> 'a TestResult.counter_ex * exn -> string
    val print_stat : ('a stat * (int,int) Hashtbl.t) -> string
    val check_cell :
      ?long:bool -> ?call:'a callback ->
      ?step:'a step -> ?handler:'a handler ->
      ?rand:Random.State.t -> 'a cell -> 'a TestResult.t
  end
end
