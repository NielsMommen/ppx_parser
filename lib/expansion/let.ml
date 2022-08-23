module type LetArgs =
  sig
    val on_fail: loc:Ppxlib.Location.t -> Ppxlib.expression
    val map_try_expr: loc:Ppxlib.Location.t -> Ppxlib.expression -> Ppxlib.expression
  end

module Make(Args: LetArgs) = 
  struct
    open Ppxlib
    open Args

    let expand_ident_to_var_pat ~loc = function
      | {pexp_desc = Pexp_ident {txt = Lident var; loc = var_loc}; pexp_loc = ident_loc; _} ->
        Ast_builder.Default.ppat_var ~loc:ident_loc {txt = var; loc = var_loc}
      | _ ->
        Err.err_pat_node ~loc "Expected an identifier."

    let expand_exprs_to_try_expr ~loc exprs =
      let rec iter exprs var_pats =
        match exprs with
        | [[%expr [%e? lhs] = [%e? rhs]]] ->
          let var_pat = expand_ident_to_var_pat ~loc:lhs.pexp_loc lhs in
          let var_pats = Ast_builder.Default.ppat_tuple ~loc (var_pat :: var_pats |> List.rev) in
          let app_expr = map_try_expr ~loc:rhs.pexp_loc [%expr [%e rhs] ppx____parser____stream____] in
          let e = [%expr try [%e app_expr] with Stream.Failure -> [%e on_fail ~loc]] in
          e, var_pats
        | e :: exprs ->
          let var_pat = expand_ident_to_var_pat ~loc:e.pexp_loc e in
          iter exprs (var_pat :: var_pats)
        | [] ->
          Err.err_expr_node ~loc "Expected an application of the form 'ident = expression'.", Err.err_pat_node ~loc "Expected an identifier"
      in
      iter exprs []

    let expand_eval = function
      | {pexp_desc = Pexp_tuple es; pexp_loc; _} ->
        expand_exprs_to_try_expr ~loc:pexp_loc es
      | e -> 
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