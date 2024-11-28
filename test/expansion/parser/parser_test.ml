open Common

let test_empty_parser () =
  let actual = f [%expr function%parser [] -> 1] in
  let expected =
    [%expr
      function ppx____parser____stream____ -> ( match [%e peek] with _ -> 1)]
  in
  check_eq ~expected ~actual "empty"

let test_empty_parser_match () =
  let actual = f [%expr match %parser s with [] -> 1] in
  let expected =
    [%expr
      (function ppx____parser____stream____ -> ( match [%e peek] with _ -> 1)) s]
  in
  check_eq ~expected ~actual "empty_match"

let test_wildcard_parser () =
  let actual = f [%expr function%parser [ _ ] -> 1] in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some _ ->
              let () = [%e junk] in
              1
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "wildcard"

let test_identity_parser () =
  let actual = f [%expr function%parser [ x ] -> x] in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some x ->
              let () = [%e junk] in
              x
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "identity"

let test_bind_stream_parser () =
  let actual = f [%expr function%parser [ 1; [%stream s] ] -> do_something s | [ [%s s] ] -> do_something s ] in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 ->
            let () = [%e junk] in
            let s = ppx____parser____stream____ in
            do_something s
          | _ ->
            let s = ppx____parser____stream____ in
            do_something s)]
  in
  check_eq ~expected ~actual "bind_stream"

let test_seq_parser () =
  let actual =
    f [%expr function%parser [ 1; 2; 3 ] -> "1" | [ 4; 2 ] -> "2" | [] -> "0"]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 -> (
              let () = [%e junk] in
              match [%e peek] with
              | Some 2 -> (
                  let () = [%e junk] in
                  match [%e peek] with
                  | Some 3 ->
                      let () = [%e junk] in
                      "1"
                  | _ -> [%e raise_err_exn])
              | _ -> [%e raise_err_exn])
          | Some 4 -> (
              let () = [%e junk] in
              match [%e peek] with
              | Some 2 ->
                  let () = [%e junk] in
                  "2"
              | _ -> [%e raise_err_exn])
          | _ -> "0")]
  in
  check_eq ~expected ~actual "seq"

let test_no_bind_call_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [ (' ' | '\t' | '\n'); (spaces as _s) ] -> () | [] -> ()]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some (' ' | '\t' | '\n') ->
              let () = [%e junk] in
              let _s =
                try spaces ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              ()
          | _ -> ())]
  in
  check_eq ~expected ~actual "no_bind_call"

let tests =
  let open Alcotest in
  [
    test_case "empty" `Quick test_empty_parser;
    test_case "empty_match" `Quick test_empty_parser_match;
    test_case "bind_stream" `Quick test_bind_stream_parser;
    test_case "pop_any" `Quick test_wildcard_parser;
    test_case "identity" `Quick test_identity_parser;
    test_case "seq" `Quick test_seq_parser;
    test_case "no_bind_call" `Quick test_no_bind_call_parser;
  ]
