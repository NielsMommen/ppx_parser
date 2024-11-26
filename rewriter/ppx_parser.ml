open Ppxlib

let ppx_parser_pat = Ast_pattern.(single_expr_payload
  (alt
    (pexp_function __ |> map1 ~f:(fun cases -> (None, cases)) )
    (pexp_match __ __ |> map2 ~f:(fun e cases -> (Some e, cases)) ))
  )

let parser_extension =
  Extension.V3.declare "parser" Extension.Context.expression ppx_parser_pat
    Ppx_parser_lib.Parser.expand_parser_from_ctxt

let parser_rule = Context_free.Rule.extension parser_extension
let () = Driver.register_transformation ~rules:[ parser_rule ] "ppx_parser"
