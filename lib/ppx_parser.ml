open Ppxlib

let ppx_parser_pat = Ast_pattern.(single_expr_payload (pexp_function __))

let parser_extension =
  Extension.V3.declare
    "parser"
    Extension.Context.expression
    ppx_parser_pat
    Expansion.Parser.expand_function_from_ctxt

let parser_rule = Context_free.Rule.extension parser_extension

let () =
  Driver.register_transformation
    ~rules:[parser_rule]
    "ppx_parser"