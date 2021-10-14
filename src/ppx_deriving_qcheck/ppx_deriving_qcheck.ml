open Ppxlib
module G = Qcheck_generators
module O = G.Observable

(** {1. ppx_deriving_qcheck} *)

(** ppx_deriving_qcheck is a ppx deriver for QCheck generators. It does a traversal
    map on type declarations annoted with [QCheck].

    Example:
    {[
    module Tree : sig
      type t

      val gen : t QCheck.Gen.t
    end = struct
      type t = Leaf | Node of int * tree * tree
      [@@deriving qcheck]
    end
    ]}
*)

(** TypeGen can serve as a derivation environment. The map can be used
    to remember how a type should be translated.

    For instance, a recursive type must be derivated to a self recursive
    call reducing the size.

    {[
    type tree = Leaf of int | Node of tree * tree
    ]}

    becomes

    {[
    let gen_tree =
      let open QCheck in
      let open Gen in
      sized
        @@ fix (fun self -> function
             | 0 -> pure Leaf
             | n ->
                 frequency
                   [
                     (1, pure Leaf);
                     ( 1,
                       map
                         (fun (gen0, gen1, gen2) -> Node (gen0, gen1, gen2))
                         (triple int (self (n / 2)) (self (n / 2))) );])
    ]}

    The type [tree] is stored in a TypeGen.t with tree <- [%expr self (n/2)]. This
    avoids the case where [tree] is derivated to [gen_tree]
*)
module TypeGen = Map.Make (struct
  type t = string

  let compare = compare
end)

(** {2. Misc. helpers} *)

let rec longident_to_str = function
  | Lident s -> s
  | Ldot (lg, s) -> Printf.sprintf "%s.%s" (longident_to_str lg) s
  | Lapply (lg1, lg2) ->
      Printf.sprintf "%s %s" (longident_to_str lg1) (longident_to_str lg2)

(** [is_rec_typ ty typ] looks for [ty] recursively in [typ].
    It for now only finds in constr and tuples. *)
let rec is_rec_typ typ_name = function
  | { ptyp_desc = Ptyp_constr ({ txt = x; _ }, _); _ } ->
      longident_to_str x = typ_name
  | { ptyp_desc = Ptyp_tuple xs; _ } -> List.exists (is_rec_typ typ_name) xs
  | _ -> false

(** [name s] produces the generator name based on [s] *)
let name s =
  let prefix = "gen" in
  match s with "t" -> prefix | s -> prefix ^ "_" ^ s

(** [pat ~loc s] creates a pattern for a generator based on {!name}. *)
let pat ~loc s =
  let (module A) = Ast_builder.make loc in
  let s = name s in
  A.pvar s

let mutually_recursive_gens ~loc gens =
  let (module A) = Ast_builder.make loc in
  let fake_gens =
    List.map
      (function
        | [%stri let [%p? pat] = [%e? expr]] ->
            let expr = [%expr fun () -> [%e expr]] in
            A.value_binding ~pat ~expr
        | _ -> assert false)
      gens
  in
  let real_gens =
    List.map
      (function
        | [%stri
            let [%p? { ppat_desc = Ppat_var { txt = s; _ }; _ } as pat] =
              [%e? _expr]] ->
            let expr = A.evar s in
            [%stri let [%p pat] = [%e expr] ()]
        | _ -> assert false)
      gens
  in

  let mutual_gens = A.pstr_value Recursive fake_gens in
  mutual_gens :: real_gens

(** {2. Generator constructors} *)

(** [gen lg] creates a generator using [lg].

    The longident can either be a:
    - Lident s: We transform to gen_s (or gen if s = "t")
    - Ldot (lg, s): We transform to qualified generator (e.g. B.gen)
*)
let gen ~loc ?(env = TypeGen.empty) lg =
  let (module A) = Ast_builder.make loc in
  match lg with
  | Lident s ->
      Option.value ~default:(name s |> A.evar) @@ TypeGen.find_opt s env
  | Ldot (lg, s) -> A.(pexp_ident (Located.mk @@ Ldot (lg, name s)))
  | Lapply (_, _) -> raise (Invalid_argument "gen received an Lapply")

(** [tree nodes leaves] creates a tree like generator *)
let tree ~loc nodes leaves =
  (G.sized ~loc) @@ (G.fix ~loc) @@
    [%expr
        fun self ->
        function
        | 0 -> [%e leaves] | n -> [%e nodes]]

(** [sized typ_name is_rec to_gen xs] uses [is_rec] to determine recursive
    nodes in [xs].

    If no recursive node is found, the type is _not_ recursive, we build a
    generator using frequency.

    However, if recursive nodes are found, we build a tree like generator using
    {!tree}.

    The function is generalized for variants and polymorphic variants:

    {[
    type t = Leaf | Node of int * t * t

    (* or *)

    type t = [`Leaf | `Node of int * t * t]
    ]}

    Therefore, [is_rec] and [to_gen] are different for variants and polymorphic
    variants. *)
let sized ~loc ~env typ_name (is_rec : 'a -> bool)
    (to_gen : ?env:expression TypeGen.t -> 'a -> expression) (xs : 'a list) =
  let (module A) = Ast_builder.make loc in
  let new_env = TypeGen.add typ_name [%expr self (n / 2)] env in
  let leaves =
    List.filter (fun x -> not (is_rec x)) xs |> List.map (to_gen ~env)
  in
  let nodes = List.filter is_rec xs in

  if List.length nodes > 0 then
    let nodes = List.map (to_gen ~env:new_env) nodes in
    let leaves = A.elist leaves |> G.frequency ~loc
    and nodes = A.elist (leaves @ nodes) |> G.frequency ~loc in
    tree ~loc nodes leaves
  else
    let gens = A.elist leaves in
    G.frequency ~loc gens


(** [tuple ~loc ?f tys] transforms list of type [tys] into a tuple generator.

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
let tuple ~loc ?(f = fun x -> x) tys =
  let tuple = Tuple.from_list tys in
  let gen = Tuple.to_gen ~loc tuple in
  let expr = Tuple.to_expr ~loc tuple |> f in
  let pat = Tuple.to_pat ~loc tuple in
  G.map ~loc pat expr gen

(** [record loc gens ?f label_decls] transforms [gens] and [label_decls] to
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
let record ~loc ~gens ?(f = fun x -> x) xs =
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
let rec gen_from_type ~loc ?(env = TypeGen.empty) ?(typ_name = "") typ =
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
          tuple ~loc tys
      | { ptyp_desc = Ptyp_constr ({ txt = ty; _ }, _); _ } ->
          let x = TypeGen.find_opt (longident_to_str ty) env in
          Option.value ~default:(gen ~loc ~env ty) x
      | { ptyp_desc = Ptyp_var s; _ } -> gen ~loc (Lident s)
      | { ptyp_desc = Ptyp_variant (rws, _, _); _ } ->
          gen_from_variant ~loc typ_name rws
      | { ptyp_desc = Ptyp_arrow (_, left, right); _ } ->
          gen_from_arrow ~loc ~env left right
      | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "This type is not supported in ppx_deriving_qcheck")

and gen_from_constr ~loc ?(env = TypeGen.empty)
    { pcd_name; pcd_args; pcd_attributes; _ } =
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
        tuple ~loc ~f:mk_constr tys
    | Pcstr_record xs ->
        let tys = List.map (fun x -> gen_from_type ~loc ~env x.pld_type) xs in
        record ~loc ~f:mk_constr ~gens:tys xs
  in

  A.pexp_tuple [ Option.value ~default:[%expr 1] weight; gen ]

and gen_from_variant ~loc typ_name rws =
  let (module A) = Ast_builder.make loc in
  let is_rec (row : row_field) : bool =
    match row.prf_desc with
    | Rinherit _ -> false
    | Rtag (_, _, typs) -> List.exists (is_rec_typ typ_name) typs
  in
  let to_gen ?env (row : row_field) : expression =
    let w =
      Attributes.weight row.prf_attributes |> Option.value ~default:[%expr 1]
    in
    let gen =
      match row.prf_desc with
      | Rinherit typ -> gen_from_type ~loc typ
      | Rtag (label, _, []) -> G.pure ~loc @@ A.pexp_variant label.txt None
      | Rtag (label, _, typs) ->
          let f expr = A.pexp_variant label.txt (Some expr) in
          tuple ~loc ~f (List.map (gen_from_type ~loc ?env) typs)
    in
    [%expr [%e w], [%e gen]]
  in
  (* the environment is emptied, a variant can not be based on other mutuals types
     containing variants

     For instance, the following type is accepted:
     {[
       type x = [`X]
       type xy = [`Y | foo]
     ]}
     However, this next is not:
     {[
       type xy = [`X | y]
       and y = [`Y]
     ]}
  *)
  let gen = sized ~loc ~env:TypeGen.empty typ_name is_rec to_gen rws in
  let typ_t = A.ptyp_constr (A.Located.mk @@ Lident typ_name) [] in
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

let gen_from_kind_variant ~loc ~env typ_name xs =
  let (module A) = Ast_builder.make loc in
  let is_rec (constr : constructor_declaration) : bool =
    match constr.pcd_args with
    | Pcstr_tuple xs -> List.exists (is_rec_typ typ_name) xs
    | _ -> false
  in
  sized ~loc ~env typ_name is_rec (gen_from_constr ~loc) xs

let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

let gen_from_type_declaration ~loc ?(env = TypeGen.empty) td =
  let name = td.ptype_name.txt in
  let pat_gen = pat ~loc name in

  let args =
    List.map
      (fun (typ, _) ->
        match typ.ptyp_desc with Ptyp_var s -> pat ~loc s | _ -> assert false)
      td.ptype_params
  in

  let gen =
    match td.ptype_kind with
    | Ptype_variant xs -> gen_from_kind_variant ~loc ~env name xs
    | Ptype_record xs ->
        let gens = List.map (fun x -> gen_from_type ~loc ~env x.pld_type) xs in
        record ~loc ~gens xs
    | _ ->
        let typ = Option.get td.ptype_manifest in
        gen_from_type ~loc ~env ~typ_name:name typ
  in
  let gen = curry_args ~loc args gen in

  [%stri let [%p pat_gen] = [%e gen]]

let derive_gen ~loc xs : structure =
  match xs with
  | _, [ x ] -> [ gen_from_type_declaration ~loc x ]
  | _, xs ->
      let (module A) = Ast_builder.make loc in
      let env =
        List.fold_left
          (fun env td ->
            let x = td.ptype_name.txt in
            let gen = name x |> A.evar in
            let expr = [%expr [%e gen] ()] in
            TypeGen.add x expr env)
          TypeGen.empty xs
      in
      let gens = List.map (gen_from_type_declaration ~loc ~env) xs in
      mutually_recursive_gens ~loc gens

(** {2. Ppxlib machinery} *)

let create_gen ~ctxt (decls : rec_flag * type_declaration list) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  derive_gen ~loc decls

let gen_expander = Deriving.Generator.V2.make_noarg create_gen

let _ = Deriving.add "qcheck" ~str_type_decl:gen_expander
