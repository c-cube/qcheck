(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

include QCheck_base_runner_intf.QCHECK_BASE_RUNNER with module QCheck_common := QCheck2 (** @inline *)
