open Ppxlib

let err_expr_node ~loc msg =
  Location.error_extensionf ~loc msg |> Ast_builder.Default.pexp_extension ~loc

let err_pat_node ~loc msg =
  Location.error_extensionf ~loc msg |> Ast_builder.Default.ppat_extension ~loc
