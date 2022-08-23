open Ppxlib

let peek ~loc = [%expr Stream.peek ppx____parser____stream____]
let junk cont ~loc = [%expr let () = Stream.junk ppx____parser____stream____ in [%e cont]]

let some pat ~loc = [%pat? Some ([%p pat])]

let expand_list_elem ~loc cont_expr = function
  | {ppat_desc = Ppat_alias ({ppat_desc = Ppat_var ({txt = call; _}); ppat_loc = call_loc; _}, as_label); ppat_loc = as_loc; _} -> 
    let var_pat = Ast_builder.Default.ppat_var ~loc:as_loc as_label in
    let fn_ident = Ast_builder.Default.pexp_ident ~loc:call_loc ({txt = Lident call; loc}) in 
    [%expr let [%p var_pat] = [%e fn_ident] ppx____parser____stream____ in [%e cont_expr]]
  | {ppat_desc = Ppat_var ({txt = call; _}); ppat_loc = call_loc; _} ->
    let var_pat = [%pat? _] in
    let fn_ident = Ast_builder.Default.pexp_ident ~loc:call_loc ({txt = Lident call; loc}) in 
    [%expr let [%p var_pat] = try [%e fn_ident] ppx____parser____stream____ with Stream.Failure -> raise (Stream.Error "Parse error.") in [%e cont_expr]]
  | {ppat_desc = Ppat_extension ({txt = "let" | "l"; _}, payload); ppat_loc; _ } ->
    let e, pat = Let.expand_let_payload_tl ~loc:ppat_loc payload in
    [%expr let [%p pat] = [%e e] in [%e cont_expr]]
  | pat ->
    [%expr match [%e peek ~loc] with Some [%p pat] -> [%e junk ~loc cont_expr] | _ -> raise (Stream.Error "Parse error.")]

let rec expand_list_seq_tl ~loc result_expr = function
  | [%pat? []] ->
    result_expr
  | [%pat? [%p? hd] :: [%p? tl]] ->
    let cont_expr = expand_list_seq_tl ~loc result_expr tl in
    expand_list_elem ~loc:hd.ppat_loc cont_expr hd
  | _ ->
    Err.err_expr_node ~loc "Expected a list of patterns."

let expand_list_seq ~loc {pc_lhs; pc_guard; pc_rhs} to_match_expr other_cases =
  match pc_lhs with
  | [%pat? [%p? hd] :: [%p? tl]] ->
    let on_match_expr = expand_list_seq_tl ~loc pc_rhs tl in
    let bind_var_w_call pat to_match_expr on_match_expr other_cases pc_guard =
      let pat = some ~loc:pat.ppat_loc pat in
      let no_match_expr = Ast_builder.Default.pexp_match ~loc to_match_expr other_cases in
      let match_case = {pc_lhs = pat; pc_guard; pc_rhs = on_match_expr} in
      let no_match_case = {pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = no_match_expr} in
      to_match_expr, match_case :: no_match_case :: []
    in
    begin match hd with
    | {ppat_desc = Ppat_extension ({txt = "let" | "l"; _}, payload); ppat_loc; _ } ->
      let to_match_expr, pat = Let.expand_let_payload_hd ~loc:ppat_loc payload in
      bind_var_w_call pat to_match_expr on_match_expr other_cases pc_guard
    | {ppat_desc = Ppat_alias ({ppat_desc = Ppat_var ({txt = call; _}); ppat_loc = call_loc; _}, as_label); ppat_loc = as_loc; _} -> 
      let var_pat = Ast_builder.Default.ppat_var ~loc:as_loc as_label in
      let fn_ident = Ast_builder.Default.pexp_ident ~loc:call_loc ({txt = Lident call; loc}) in 
      let to_match_expr = [%expr try Some ([%e fn_ident] ppx____parser____stream____) with Stream.Failure -> None] in
      bind_var_w_call var_pat to_match_expr on_match_expr other_cases pc_guard
    | pat ->
      let pat = some ~loc:pat.ppat_loc pat in
      let on_match_expr = junk ~loc on_match_expr in
      to_match_expr, {pc_lhs = pat; pc_guard; pc_rhs = on_match_expr} :: other_cases
    end
  | _ -> 
    Err.err_expr_node ~loc:pc_lhs.ppat_loc "Expected a case where the left-hand side is a list of patterns.", other_cases

let expand_function_cases ~loc cases =
  let pk = [%expr Stream.peek ppx____parser____stream____] in
  let rec iter cases =
    match cases with
    | {pc_lhs = [%pat? []]; pc_guard = None; pc_rhs} :: [] ->
      let case = {pc_lhs = [%pat? _]; pc_guard = None; pc_rhs} in
      pk, [case]
    | [] ->
      let case = {pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = [%expr raise Stream.Failure]} in
      pk, [case]
    | case :: cases_rest ->
      let to_match_expr, cases = iter cases_rest in
      expand_list_seq ~loc:case.pc_lhs.ppat_loc case to_match_expr cases
  in
  iter cases

let expand_function ~loc cases =
  let to_match_expr, cases = expand_function_cases ~loc cases in
  let match_expr = Ast_builder.Default.pexp_match ~loc to_match_expr cases in
  [%expr function ppx____parser____stream____ -> [%e match_expr]]

let expand_function_from_ctxt ~ctxt cases = 
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  expand_function ~loc cases
