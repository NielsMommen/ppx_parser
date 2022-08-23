module type LetArgs =
  sig
    val on_fail: loc:Ppxlib.Location.t -> Ppxlib.expression
    val map_try_expr: loc:Ppxlib.Location.t -> Ppxlib.expression -> Ppxlib.expression
  end

open Ppxlib

let rec map_expr_to_pat ~loc = function
  | {pexp_desc = Pexp_ident {txt = Lident var; loc = var_loc}; pexp_loc = ident_loc; _} ->
    Ast_builder.Default.ppat_var ~loc:ident_loc {txt = var; loc = var_loc}
  | {pexp_desc = Pexp_tuple es; pexp_loc = loc; _} ->
    let vars = es |> List.map (fun e -> map_expr_to_pat ~loc:e.pexp_loc e) in
    Ast_builder.Default.ppat_tuple ~loc vars
  | {pexp_desc = Pexp_record (es, None); _} ->
    let vars = es |> List.map (fun (l, e) -> l, map_expr_to_pat ~loc:e.pexp_loc e) in
    Ast_builder.Default.ppat_record ~loc vars Closed
  | {pexp_desc = Pexp_constraint (e, ct); pexp_loc = loc; _} ->
    let var = map_expr_to_pat ~loc:e.pexp_loc e in
    Ast_builder.Default.ppat_constraint ~loc var ct
  | _ ->
    Err.err_pat_node ~loc "Expected an identifier 'x', tuple '( )', record '{ }' or constraint 'a: b'."

module Make(Args: LetArgs) = 
  struct
    open Args

    let try_expr_nd_vars ~loc app_expr var_pats =
      let var_pats = Ast_builder.Default.ppat_tuple ~loc var_pats in
      let app_expr = map_try_expr ~loc:app_expr.pexp_loc [%expr [%e app_expr] ppx____parser____stream____] in
      let e = [%expr try [%e app_expr] with Stream.Failure -> [%e on_fail ~loc]] in
      e, var_pats

    let expand_exprs_to_try_expr ~loc exprs =
      let rec iter exprs var_pats =
        match exprs with
        | [[%expr [%e? lhs] = [%e? rhs]]] ->
          let var_pat = map_expr_to_pat ~loc:lhs.pexp_loc lhs in
          let var_pats = var_pat :: var_pats |> List.rev in
          try_expr_nd_vars ~loc rhs var_pats
        | e :: exprs ->
          let var_pat = map_expr_to_pat ~loc:e.pexp_loc e in
          iter exprs (var_pat :: var_pats)
        | [] ->
          Err.err_expr_node ~loc "Expected an application of the form 'ident = expression'.", [%pat? _]
      in
      iter exprs []

    let expand_eval = function
      | {pexp_desc = Pexp_tuple es; pexp_loc; _} ->
        (* bind result of call to multiple vars *)
        expand_exprs_to_try_expr ~loc:pexp_loc es
      | ({pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident ({txt = Lident lbl; _}); _}, _); pexp_loc = loc; _}) as e when lbl <> "=" ->
        (* call without binding its result *)
        try_expr_nd_vars ~loc e [[%pat? _]]
      | e -> 
        (* bind result of call to one var *)
        expand_exprs_to_try_expr ~loc:e.pexp_loc [e]

    let expand_let_payload ~loc = function
      | PStr [{pstr_desc = Pstr_eval (e, []); _}] ->
        expand_eval e
      | _ ->
        Err.err_expr_node ~loc "Invalid '%%let' payload", [%pat? _]

end

module LetHd = Make(
  struct 
    let on_fail ~loc = [%expr None]
    let map_try_expr ~loc e = [%expr Some ([%e e])]
  end
)

module LetTl = Make(
  struct
    let on_fail ~loc = [%expr raise (Stream.Error "Parse error.")]
    let map_try_expr ~loc:_ e = e
  end
)

let expand_let_payload_hd = LetHd.expand_let_payload
let expand_let_payload_tl = LetTl.expand_let_payload