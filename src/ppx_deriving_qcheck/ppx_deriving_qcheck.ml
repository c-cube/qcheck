open Ppxlib
module G = Qcheck_generators
module O = G.Observable

(** {1. ppx_deriving_qcheck} *)

(** ppx_deriving_qcheck is a ppx deriver for QCheck generators. It does a
    traversal map on type declarations annoted with [QCheck].

    Example:
    {[
    module Tree : sig
      type t

      val gen : t QCheck.Gen.t
    end = struct
      type t = Leaf | Node of int * t * t
      [@@deriving qcheck]
    end
    ]}
*)

(** {2. Misc. helpers} *)

(** [name s] produces the generator name based on [s] *)
let name ?(sized = false) s =
  let prefix = "gen" in
  (match s with "t" -> prefix | s -> prefix ^ "_" ^ s) ^
    (if sized then "_sized" else "")

(** [pat ~loc s] creates a pattern for a generator based on {!name}. *)
let pat ~loc ?sized s =
  let (module A) = Ast_builder.make loc in
  let s = name ?sized s in
  A.pvar s

(** {2. Recursive generators} *)

(** Recursive generators must be treated separatly:

    {[
    type 'a list = Cons of 'a * 'a list | Nil
    ]}

    becomes:

    {[
    let rec gen_list_sized gen_a n =
      match n with
      | 0 -> pure Nil
      | n -> map2 (fun x xs -> Cons (x, xs) gen_a (gen_list_sized gen_a (n/2))

    let gen_list_sized gen_a = sized @@ (gen_list_sized gen_a)
    ]}

    In the basic derivation {[ 'a list ]} would be translated to {[gen_list]}.
    However, we want the generator to call itsef.
 *)

module Env = struct
  (** [env] contains:
      - the list of recursive types during the derivation
      - the list of types to derive (i.e. mutual types)
      - the current type to derive *)
  type env = {
      rec_types : string list;
      curr_types : string list;
      curr_type : string;
    }

  let is_rec env x = List.mem x env.rec_types
end

let rec longident_to_str = function
  | Lident s -> s
  | Ldot (lg, s) -> Printf.sprintf "%s.%s" (longident_to_str lg) s
  | Lapply (lg1, lg2) ->
      Printf.sprintf "%s %s" (longident_to_str lg1) (longident_to_str lg2)

let rec is_rec_typ env = function
  | { ptyp_desc = Ptyp_constr ({ txt = x; _ }, _); _ } ->
     List.exists (fun typ_name -> longident_to_str x = typ_name) env.Env.curr_types
  | { ptyp_desc = Ptyp_tuple xs; _ } -> List.exists (is_rec_typ env) xs
  | { ptyp_desc = Ptyp_variant (rws, _, _); _ } ->
     List.exists (is_rec_row_field env) rws
  | _ -> false

and is_rec_row_field env rw =
  match rw.prf_desc with
  | Rtag (lab, _, cts) ->
     List.exists (fun typ_name -> lab.txt = typ_name) env.Env.curr_types ||
       List.exists (is_rec_typ env) cts
  | Rinherit ct -> is_rec_typ env ct

let is_rec_constr_decl env cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_rec_typ env) cts
  | _ -> false

(** [is_rec_type_decl env typ] looks for elements of [env.curr_types]
    recursively in [typ]. *)
let is_rec_type_decl env typ =
  let in_type_kind =
    match typ.ptype_kind with
    | Ptype_variant cstrs -> List.exists (is_rec_constr_decl env) cstrs
    | _ -> false
  in
  let in_manifest =
    match typ.ptype_manifest with
    | Some x -> is_rec_typ env x
    | None -> false
  in
  in_type_kind || in_manifest

(** {2. Generator constructors} *)

(** [gen_longident lg args] creates a generator using [lg].

    The longident can either be a:
    - Lident s: We transform to gen_s (or gen if s = "t")
    - Ldot (lg, s): We transform to qualified generator (e.g. B.gen)
*)
let gen_longident ~loc ~env lg args =
  let (module A) = Ast_builder.make loc in
  match lg with
  | Lident s ->
     if Env.is_rec env s then
       name ~sized:true s |> A.evar |>
         Args.apply_args ~loc args |>
         Args.apply_args ~loc [ [%expr (n / 2)] ]
     else
       name s |> A.evar |> Args.apply_args ~loc args
  | Ldot (lg, s) ->
     A.(pexp_ident (Located.mk @@ Ldot (lg, name s))) |>
       Args.apply_args ~loc args
  | Lapply (_, _) -> raise (Invalid_argument "gen received an Lapply")

(** [gen_sized typ_name is_rec to_gen xs] uses [is_rec] to determine recursive
    nodes in [xs].

    If no recursive node is found, the type is _not_ recursive, we build a
    generator using frequency.

    However, if recursive nodes are found, we build a tree like generator using
    {!gen_sized}.

    The function is generalized for variants and polymorphic variants:

    {[
    type t = Leaf | Node of int * t * t

    (* or *)

    type t = [`Leaf | `Node of int * t * t]
    ]}

    Therefore, [is_rec] and [to_gen] are different for variants and polymorphic
    variants. *)
let gen_sized ~loc (is_rec : 'a -> bool) (to_gen : 'a -> expression) (xs : 'a list) =
  let (module A) = Ast_builder.make loc in
  let leaves =
    List.filter (fun x -> not (is_rec x)) xs |> List.map to_gen
  in
  let nodes = List.filter is_rec xs in

  if List.length nodes = 0 then
    G.frequency ~loc (A.elist leaves)
  else if List.length leaves = 0 then
    let nodes = List.map to_gen nodes in
    G.frequency ~loc (A.elist nodes)
  else
    let nodes = List.map to_gen nodes in
    let leaves = A.elist leaves |> G.frequency ~loc
    and nodes = A.elist (leaves @ nodes) |> G.frequency ~loc in
    [%expr
        match n with
        | 0 -> [%e leaves]
        | n -> [%e nodes]
    ]

(** [gen_tuple ~loc ?f tys] transforms list of type [tys] into a tuple generator.

    [f] can be used to transform tuples, for instance:
    {[
    type t = Foo of int * int
    ]}

    Without [f]:
    {[
    let gen = QCheck.Gen.(map (fun (x, y) -> (x, y)) (pair int int))
    ]}

    With [f], building Foo:
    {[
    let gen = QCheck.Gen.(map (fun (x, y) -> Foo (x, y)) (pair int int))
    ]}
*)
let gen_tuple ~loc ?(f = fun x -> x) tys =
  let tuple = Tuple.from_list tys in
  let gen = Tuple.to_gen ~loc tuple in
  let expr = Tuple.to_expr ~loc tuple |> f in
  let pat = Tuple.to_pat ~loc tuple in
  G.map ~loc pat expr gen

(** [gen_record loc gens ?f label_decls] transforms [gens] and [label_decls] to
    a record generator.

    Similarly to {!gen_tuple}, we can use [f] to transform records, for instance:
    {[
    type t = Foo of { left : int; right : int }
    ]}

    Without [f]:
    {[
    let gen = QCheck.Gen.(map (fun (x, y) -> {left = x; right = y}) (pair int int))
    ]}

    With [f], building Foo:
    {[
    let gen = QCheck.Gen.(map (fun (x, y) -> Foo {left = x; right = y}) (pair int int))
    ]}

*)
let gen_record ~loc ~gens ?(f = fun x -> x) xs =
  let (module A) = Ast_builder.make loc in
  let tuple = Tuple.from_list gens in
  let gen = Tuple.to_gen ~loc tuple in
  let pat = Tuple.to_pat ~loc tuple in
  (* TODO: this should be handled in {!Tuple} *)
  let gens =
    List.mapi
      (fun i _ ->
        let s = Printf.sprintf "gen%d" i in
        A.evar s)
      gens
  in
  let fields =
    List.map2
      (fun { pld_name; _ } value ->
        (A.Located.mk @@ Lident pld_name.txt, value))
      xs gens
  in
  let expr = A.pexp_record fields None |> f in

  G.map ~loc pat expr gen

(** {2. Core derivation} *)

(** [gen_from_type typ] performs the AST traversal and derivation to qcheck generators *)
let rec gen_from_type ~loc ~env typ =
  Option.value (Attributes.gen typ)
    ~default:
      (match typ with
      | [%type: unit] -> G.unit loc
      | [%type: int] -> G.int loc
      | [%type: string] | [%type: String.t] -> G.string loc
      | [%type: char] -> G.char loc
      | [%type: bool] -> G.bool loc
      | [%type: float] -> G.float loc
      | [%type: int32] | [%type: Int32.t] -> G.int32 loc
      | [%type: int64] | [%type: Int64.t] -> G.int64 loc
      | [%type: [%t? typ] option] -> G.option ~loc (gen_from_type ~loc ~env typ)
      | [%type: [%t? typ] list] -> G.list ~loc (gen_from_type ~loc ~env typ)
      | [%type: [%t? typ] array] -> G.array ~loc (gen_from_type ~loc ~env typ)
      | { ptyp_desc = Ptyp_tuple typs; _ } ->
          let tys = List.map (gen_from_type ~loc ~env) typs in
          gen_tuple ~loc tys
      | { ptyp_desc = Ptyp_constr ({ txt = ty; _ }, args); _ } ->
          let args = List.map (gen_from_type ~loc ~env) args in
          gen_longident ~loc ~env ty args
      | { ptyp_desc = Ptyp_var s; _ } ->
          gen_longident ~loc ~env (Lident s) []
      | { ptyp_desc = Ptyp_variant (rws, _, _); _ } ->
          gen_from_variant ~loc ~env rws
      | { ptyp_desc = Ptyp_arrow (_, left, right); _ } ->
          gen_from_arrow ~loc ~env left right
      | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "This type is not supported in ppx_deriving_qcheck")

and gen_from_constr ~loc ~env { pcd_name; pcd_args; pcd_attributes; _ } =
  let (module A) = Ast_builder.make loc in
  let constr_decl =
    A.constructor_declaration ~name:pcd_name ~args:pcd_args ~res:None
  in
  let mk_constr expr = A.econstruct constr_decl (Some expr) in
  let weight = Attributes.weight pcd_attributes in
  let gen =
    match pcd_args with
    | Pcstr_tuple [] | Pcstr_record [] ->
        G.pure ~loc @@ A.econstruct constr_decl None
    | Pcstr_tuple xs ->
        let tys = List.map (gen_from_type ~loc ~env) xs in
        gen_tuple ~loc ~f:mk_constr tys
    | Pcstr_record xs ->
        let tys = List.map (fun x -> gen_from_type ~loc ~env x.pld_type) xs in
        gen_record ~loc ~f:mk_constr ~gens:tys xs
  in

  A.pexp_tuple [ Option.value ~default:[%expr 1] weight; gen ]

and gen_from_variant ~loc ~env rws =
  let (module A) = Ast_builder.make loc in
  let is_rec = is_rec_row_field env in
  let to_gen (row : row_field) : expression =
    let w =
      Attributes.weight row.prf_attributes |> Option.value ~default:[%expr 1]
    in
    let gen =
      match row.prf_desc with
      | Rinherit typ -> gen_from_type ~loc ~env typ
      | Rtag (label, _, []) -> G.pure ~loc @@ A.pexp_variant label.txt None
      | Rtag (label, _, typs) ->
          let f expr = A.pexp_variant label.txt (Some expr) in
          gen_tuple ~loc ~f (List.map (gen_from_type ~loc ~env) typs)
    in
    [%expr [%e w], [%e gen]]
  in
  let gen = gen_sized ~loc is_rec to_gen rws in
  let typ_t = A.ptyp_constr (A.Located.mk @@ Lident env.curr_type) [] in
  let typ_gen = A.Located.mk @@ Lident G.ty in
  let typ = A.ptyp_constr typ_gen [ typ_t ] in
  [%expr ([%e gen] : [%t typ])]

and gen_from_arrow ~loc ~env left right =
  let rec observable = function
    | [%type: unit] -> O.unit loc
    | [%type: bool] -> O.bool loc
    | [%type: int] -> O.int loc
    | [%type: float] -> O.float loc
    | [%type: string] -> O.string loc
    | [%type: char] -> O.char loc
    | [%type: [%t? typ] option] -> O.option ~loc (observable typ)
    | [%type: [%t? typ] array] -> O.array ~loc (observable typ)
    | [%type: [%t? typ] list] -> O.list ~loc (observable typ)
    | { ptyp_desc = Ptyp_tuple xs; _ } ->
        let obs = List.map observable xs in
        Tuple.from_list obs |> Tuple.to_obs ~loc
    | { ptyp_loc = loc; _ } ->
        Ppxlib.Location.raise_errorf ~loc
          "This type is not supported in ppx_deriving_qcheck"
  in
  let rec aux = function
    | { ptyp_desc = Ptyp_arrow (_, x, xs); _ } ->
        let res, xs = aux xs in
        let obs = observable x in
        (res, [%expr [%e obs] @-> [%e xs]])
    | x -> (gen_from_type ~loc ~env x, [%expr o_nil])
  in
  let x, obs = aux right in
  (* TODO: export this in qcheck_generators for https://github.com/c-cube/qcheck/issues/190 *)
  let arb = [%expr QCheck.make [%e x]] in
  [%expr
    QCheck.fun_nary QCheck.Tuple.([%e observable left] @-> [%e obs]) [%e arb]
    |> QCheck.gen]

(** [gen_from_type_declaration loc td] creates a generator from the type declaration.

    It returns either `Recursive or `Normal.

    - `Normal of expression:
    The derived generator is not recursive, we return only the generator.

    - `Recursive of expression * expression
    The derived generator was recursive (i.e. val gen : n -> t Gen.t), we return
    the sized generator version, and a normal generator using this last with
    [Gen.sized].
*)
let gen_from_type_declaration ~loc ~env td =
  let (module A) = Ast_builder.make loc in
  let ty = env.Env.curr_type in
  let is_rec = Env.is_rec env ty in

  let args =
    List.map
      (fun (typ, _) ->
        match typ.ptyp_desc with
        | Ptyp_var s -> (pat ~loc s, name s |> A.evar)
        | _ -> assert false)
      td.ptype_params
  in
  let (args_pat, args_expr) = List.split args in

  let gen =
    match td.ptype_kind with
    | Ptype_variant xs ->
        let is_rec cd = is_rec_constr_decl env cd in
        gen_sized ~loc is_rec (gen_from_constr ~loc ~env) xs
    | Ptype_record xs ->
        let gens = List.map (fun x -> gen_from_type ~loc ~env x.pld_type) xs in
        gen_record ~loc ~gens xs
    | _ ->
        let typ = Option.get td.ptype_manifest in
        gen_from_type ~loc ~env typ
  in

  let pat_gen = pat ~loc ty in
  if not is_rec then
    let gen = Args.curry_args ~loc args_pat gen in
    `Normal [%stri let [%p pat_gen] = [%e gen]]
  else
    let gen = Args.curry_args ~loc (args_pat @ [A.pvar "n"]) gen in
    let pat_gen_sized = pat ~loc ~sized:true ty in
    let gen_sized = name ~sized:true ty |> A.evar in
    let gen_normal =
      Args.curry_args ~loc args_pat
        (G.sized ~loc (Args.apply_args ~loc args_expr gen_sized))
    in
    `Recursive (
        [%stri let rec [%p pat_gen_sized] = [%e gen]],
        [%stri let [%p pat_gen] = [%e gen_normal]]
      )

let mutually_recursive_gens ~loc gens =
  let (module A) = Ast_builder.make loc in
  let to_mutualize_gens =
    List.map (function
        | `Recursive (x, _) -> x
        | `Normal x -> x) gens
  in
  let normal_gens =
    List.filter_map (function
        | `Recursive (_, x) -> Some x
        | `Normal _ -> None) gens
  in
  let gens =
    List.map (function
        | [%stri let [%p? pat] = [%e? expr]]
          | [%stri let rec [%p? pat] = [%e? expr]] ->
           A.value_binding ~pat ~expr
        | _ -> assert false) to_mutualize_gens
  in
  let mutual_gens = A.pstr_value Recursive gens in
  mutual_gens :: normal_gens

(** [derive_gen ~loc xs] creates generators for type declaration in [xs]. *)
let derive_gen ~loc (xs : rec_flag * type_declaration list) : structure =
  let open Env in
  let add_if_rec env typ x =
    if is_rec_type_decl env typ then
      { env with rec_types = x :: env.rec_types}
    else env
  in
  match xs with
  | (_, [ x ]) ->
     let typ_name = x.ptype_name.txt in
     let env = { curr_type = typ_name; rec_types = []; curr_types = [typ_name] } in
     let env = add_if_rec env x typ_name in
     (match gen_from_type_declaration ~loc ~env x with
      | `Recursive (gen_sized, gen) -> [gen_sized; gen]
      | `Normal gen -> [gen])
  | _, xs ->
     let typ_names = List.map (fun x -> x.ptype_name.txt) xs in
     let env = { curr_type = ""; rec_types = []; curr_types = typ_names } in
     let env =
       List.fold_left
         (fun env x -> add_if_rec env x x.ptype_name.txt)
         env xs
     in
     let gens =
       List.map (fun x ->
           let env = { env with curr_type = x.ptype_name.txt }in
           gen_from_type_declaration ~loc ~env x) xs
     in
     mutually_recursive_gens ~loc gens

(** {2. Ppxlib machinery} *)

let create_gen ~ctxt (decls : rec_flag * type_declaration list) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  derive_gen ~loc decls

let gen_expander = Deriving.Generator.V2.make_noarg create_gen

let _ = Deriving.add "qcheck" ~str_type_decl:gen_expander
