open Ppxlib

(** [find_first_attribute xs name] returns the first attribute found in [xs]
    named [name] *)
let find_attribute_opt xs name =
  List.find_opt (fun attribute -> attribute.attr_name.txt = name) xs

let get_expr_payload x =
  match x.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> Some [%expr [%e e]]
  | _ -> None

let gen ct =
  Option.fold ~none:None ~some:get_expr_payload
  @@ find_attribute_opt ct.ptyp_attributes "gen"

let weight xs =
  Option.fold ~none:None ~some:get_expr_payload
  @@ find_attribute_opt xs "weight"
