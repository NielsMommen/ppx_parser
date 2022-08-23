let () =
  let open Alcotest in
  run "ppx_parser" [
    "%parser", Parser_test.tests;
    "%let", Let_test.tests;
    "calc1", Calc_test.tests1;
    "calc2", Calc_test.tests2;
    "parsers", Parsers_test.tests;
  ]
