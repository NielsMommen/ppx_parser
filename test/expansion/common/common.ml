open Ppxlib

let testble =
  Alcotest.testable
    (fun fmt s -> Format.pp_print_string fmt s)
    (fun a b -> String.equal a b)

let check_eq ~expected ~actual test_name =
  let expected = Pprintast.string_of_expression expected in
  let actual = Pprintast.string_of_expression actual in
  Alcotest.(check' testble) ~msg:test_name ~expected ~actual

let loc = Ppxlib.Location.none
let raise_fail_exn = [%expr raise Stream.Failure]
let raise_err_exn = [%expr raise (Stream.Error "Parse error.")]
let peek = [%expr Stream.peek ppx____parser____stream____]
let junk = [%expr Stream.junk ppx____parser____stream____]

let parser_ext_pat =
  Ast_pattern.(
    pexp_extension (extension (string "parser") Ppx_parser.ppx_parser_pat))

let f e =
  Ast_pattern.parse parser_ext_pat e.pexp_loc
    ~on_error:(fun () ->
      Ppx_parser_lib.Err.err_expr_node ~loc:e.pexp_loc "Invalid test pattern.")
    e
    (Ppx_parser_lib.Parser.expand_function ~loc)
