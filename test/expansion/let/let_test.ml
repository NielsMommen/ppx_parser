open Common

let test_alias_parser () = 
  let actual = f [%expr  
    function%parser
    | [1 as x; 2; 3 as y] -> x + y
    | [5 as x] -> x
  ]
  in
  let expected = [%expr
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some (1 as x) ->
        let () = [%e junk] in
        (match [%e peek] with
        | Some 2 ->
          let () = [%e junk] in
          (match [%e peek] with
          | Some (3 as y) ->
            let () = [%e junk] in
            x + y
          | _ -> [%e raise_err_exn])
        | _ -> [%e raise_err_exn])
      | Some (5 as x) ->
        let () = [%e junk] in
        x
      | _ -> [%e raise_fail_exn]
  ]
  in
  check_eq ~expected ~actual "alias"

let test_call_alias_parser () =
  let actual = f [%expr  
    function%parser
    | [1; some_call as x] -> x + 1
  ]
  in
  let expected = [%expr   
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some 1 ->
        let () = [%e junk] in
        let x = some_call ppx____parser____stream____ in
        x + 1
      | _ ->
        [%e raise_fail_exn]
  ]
  in
  check_eq ~expected ~actual "call_alias"

let test_let_ext_parser () =
  let actual = f [%expr  
    function%parser
    | [1; [%let x = some_call]] -> x
    | [2; [%l x = some_call]] -> x
  ]
  in
  let expected = [%expr  
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some 1 ->
        let () = [%e junk] in
        let x = try some_call ppx____parser____stream____ with Stream.Failure -> [%e raise_err_exn] in
        x
      | Some 2 ->
        let () = [%e junk] in
        let x = try some_call ppx____parser____stream____ with Stream.Failure -> [%e raise_err_exn] in
        x
      | _ -> 
        [%e raise_fail_exn]
  ]
  in
  check_eq ~expected ~actual "let_ext"

let anom_parser = f [%expr  
  function%parser
      | [1; 2] -> 1
      | [] -> 0
]

let test_anom_parser_parser () =
  let actual = f [%expr 
    function%parser
    | [0; [%l x = [%e anom_parser]]] -> x
    | [] -> -1
  ]
  in
  let expected = [%expr
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some 0 ->
        let () = [%e junk] in
        let x = 
          try
          (function ppx____parser____stream____ ->
            match [%e peek] with
            | Some 1 ->
              let () = [%e junk] in
              (match [%e peek] with
              | Some 2 -> 
                let () = [%e junk] in
                1
              | _ -> [%e raise_err_exn])
            | _ -> 0) ppx____parser____stream____
          with Stream.Failure -> [%e raise_err_exn]
        in
        x
      | _ -> -1
  ]
  in
  check_eq ~expected ~actual "anom_parser"

let test_let_parser_parser () =
  let actual = f [%expr 
    function%parser
    | [0; [%l x = 
      let my_parser = [%e anom_parser] in
      my_parser
    ]] -> x
    | [] -> -1
  ]
  in
  let expected = [%expr
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some 0 ->
        let () = [%e junk] in
        let x =
          try
          (let my_parser = function ppx____parser____stream____ ->
            match [%e peek] with
            | Some 1 ->
              let () = [%e junk] in
              (match [%e peek] with
              | Some 2 -> 
                let () = [%e junk] in
                1
              | _ -> [%e raise_err_exn])
            | _ -> 0 
          in
          my_parser) ppx____parser____stream____
          with Stream.Failure -> [%e raise_err_exn]
        in
        x
      | _ -> -1
  ]
  in
  check_eq ~expected ~actual "let_parser"

let test_multi_bind_parser () =
  let actual = f [%expr
    function%parser
    | [1; [%l x, y = some_parser]] -> x + y
  ]
  in
  let expected = [%expr
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some 1 ->
        let () = [%e junk] in
        let x, y = try some_parser ppx____parser____stream____ with Stream.Failure -> [%e raise_err_exn] in
        x + y
      | _ -> [%e raise_fail_exn]
  ]
  in
  check_eq ~expected ~actual "multi_bind"

let test_multi_bind_w_args_parser () =
  let actual = f [%expr
    function%parser
    | [1 as l; [%l x, y = some_parser l 2 3]] -> x - y
  ]
  in
  let expected = [%expr
    function ppx____parser____stream____ ->
      match [%e peek] with
      | Some (1 as l) ->
        let () = [%e junk] in
        let x, y = try (some_parser l 2 3) ppx____parser____stream____ with Stream.Failure -> [%e raise_err_exn]in
        x - y
      | _ -> [%e raise_fail_exn]
  ]
  in
  check_eq ~expected ~actual "multi_bind_w_args"

let tests = let open Alcotest in [
  test_case "alias" `Quick test_alias_parser;
  test_case "call_alias" `Quick test_call_alias_parser;
  test_case "let_ext" `Quick test_let_ext_parser;
  test_case "anom_parser" `Quick test_anom_parser_parser;
  test_case "let_parser" `Quick test_let_parser_parser;
  test_case "multi_bind" `Quick test_multi_bind_parser;
  test_case "multi_bind_w_args" `Quick test_multi_bind_w_args_parser;
]