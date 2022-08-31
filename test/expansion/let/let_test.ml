open Common

let test_alias_parser () =
  let actual =
    f
      [%expr
        function%parser [ (1 as x); 2; (3 as y) ] -> x + y | [ (5 as x) ] -> x]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some (1 as x) -> (
              let () = [%e junk] in
              match [%e peek] with
              | Some 2 -> (
                  let () = [%e junk] in
                  match [%e peek] with
                  | Some (3 as y) ->
                      let () = [%e junk] in
                      x + y
                  | _ -> [%e raise_err_exn])
              | _ -> [%e raise_err_exn])
          | Some (5 as x) ->
              let () = [%e junk] in
              x
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "alias"

let test_call_alias_parser () =
  let actual = f [%expr function%parser [ 1; (some_call as x) ] -> x + 1] in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 ->
              let () = [%e junk] in
              let x =
                try some_call ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x + 1
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "call_alias"

let test_let_ext_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [ 1; [%let x = some_call] ] -> x | [ 2; [%l x = some_call] ] -> x]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 ->
              let () = [%e junk] in
              let x =
                try some_call ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x
          | Some 2 ->
              let () = [%e junk] in
              let x =
                try some_call ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "let_ext"

let anom_parser = f [%expr function%parser [ 1; 2 ] -> 1 | [] -> 0]

let test_anom_parser_parser () =
  let actual =
    f [%expr function%parser [ 0; [%l x = [%e anom_parser]] ] -> x | [] -> -1]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 0 ->
              let () = [%e junk] in
              let x =
                try
                  (function
                    | ppx____parser____stream____ -> (
                        match [%e peek] with
                        | Some 1 -> (
                            let () = [%e junk] in
                            match [%e peek] with
                            | Some 2 ->
                                let () = [%e junk] in
                                1
                            | _ -> [%e raise_err_exn])
                        | _ -> 0))
                    ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x
          | _ -> -1)]
  in
  check_eq ~expected ~actual "anom_parser"

let test_let_parser_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [
            0;
            [%l
              x
              =
              let my_parser = [%e anom_parser] in
              my_parser];
          ] ->
            x
        | [] -> -1]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 0 ->
              let () = [%e junk] in
              let x =
                try
                  (let my_parser = function
                     | ppx____parser____stream____ -> (
                         match [%e peek] with
                         | Some 1 -> (
                             let () = [%e junk] in
                             match [%e peek] with
                             | Some 2 ->
                                 let () = [%e junk] in
                                 1
                             | _ -> [%e raise_err_exn])
                         | _ -> 0)
                   in
                   my_parser)
                    ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x
          | _ -> -1)]
  in
  check_eq ~expected ~actual "let_parser"

let test_multi_bind_parser () =
  let actual =
    f [%expr function%parser [ 1; [%l x, y = some_parser] ] -> x + y]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 ->
              let () = [%e junk] in
              let x, y =
                try some_parser ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x + y
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "multi_bind"

let test_multi_bind_w_args_parser () =
  let actual =
    f
      [%expr
        function%parser [ (1 as l); [%l x, y = some_parser l 2 3] ] -> x - y]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some (1 as l) ->
              let () = [%e junk] in
              let x, y =
                try (some_parser l 2 3) ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              x - y
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "multi_bind_w_args"

let test_parser_calls_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [ (op as t); (elem as y); [%l r = op_elem (t x y)] ] -> r | [] -> x]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match
            try Some (op ppx____parser____stream____)
            with Stream.Failure -> None
          with
          | Some t ->
              let y =
                try elem ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              let r =
                try (op_elem (t x y)) ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              r
          | _ -> x)]
  in
  check_eq ~expected ~actual "parser_calls"

let test_bind_pats_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [ [%let 1 = get_const_one]; [%let (a, 2) = get_tuple_one_two] ] -> a
        | [
            [%let { rt_b; rt_a } = get_rec_tuple_one_two];
            0;
            [%let Some x = get_some_one];
          ] ->
            rt_b + rt_a + x
        | [ 3; [%let None = get_none]; [%let Int x = get_int_lit_one] ] -> x
        | [ 4; [%let [| a; b; c |] = get_arr]; [%let lazy x = get_lazy] ] ->
            a + b + c + x
        | [ 5; 4; 3; 2; 1 ] -> 0]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match
            try Some (get_const_one ppx____parser____stream____)
            with Stream.Failure -> None
          with
          | Some 1 ->
              let a, 2 =
                try get_tuple_one_two ppx____parser____stream____
                with Stream.Failure -> [%e raise_err_exn]
              in
              a
          | _ -> (
              match
                try Some (get_rec_tuple_one_two ppx____parser____stream____)
                with Stream.Failure -> None
              with
              | Some { rt_b; rt_a } -> (
                  match [%e peek] with
                  | Some 0 ->
                      let () = [%e junk] in
                      let (Some x) =
                        try get_some_one ppx____parser____stream____
                        with Stream.Failure -> [%e raise_err_exn]
                      in
                      rt_b + rt_a + x
                  | _ -> [%e raise_err_exn])
              | _ -> (
                  match [%e peek] with
                  | Some 3 ->
                      let () = [%e junk] in
                      let None =
                        try get_none ppx____parser____stream____
                        with Stream.Failure -> [%e raise_err_exn]
                      in
                      let (Int x) =
                        try get_int_lit_one ppx____parser____stream____
                        with Stream.Failure -> [%e raise_err_exn]
                      in
                      x
                  | Some 4 ->
                      let () = [%e junk] in
                      let [| a; b; c |] =
                        try get_arr ppx____parser____stream____
                        with Stream.Failure -> [%e raise_err_exn]
                      in
                      let (lazy x) =
                        try get_lazy ppx____parser____stream____
                        with Stream.Failure -> [%e raise_err_exn]
                      in
                      a + b + c + x
                  | Some 5 -> (
                      let () = [%e junk] in
                      match [%e peek] with
                      | Some 4 -> (
                          let () = [%e junk] in
                          match [%e peek] with
                          | Some 3 -> (
                              let () = [%e junk] in
                              match [%e peek] with
                              | Some 2 -> (
                                  let () = [%e junk] in
                                  match [%e peek] with
                                  | Some 1 ->
                                      let () = [%e junk] in
                                      0
                                  | _ -> [%e raise_err_exn])
                              | _ -> [%e raise_err_exn])
                          | _ -> [%e raise_err_exn])
                      | _ -> [%e raise_err_exn])
                  | _ -> [%e raise_fail_exn])))]
  in
  check_eq ~expected ~actual "bind_pats"

let test_or_pat_alias_parser () =
  let actual =
    f [%expr function%parser [ ((' ' | '\t' | '\n') as c) ] -> Char.code c]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some ((' ' | '\t' | '\n') as c) ->
              let () = [%e junk] in
              Char.code c
          | _ -> [%e raise_fail_exn])]
  in
  check_eq ~expected ~actual "or_pat_alias"

let test_peek_call_alternate_parser () =
  let actual =
    f
      [%expr
        function%parser
        | [ 1; 2 ] -> 3
        | [ (some_call as c); 4 ] -> c + 5
        | [ 6; 7 ] -> 8
        | [ (some_call as a); (some_call as b) ] -> a + b + 9]
  in
  let expected =
    [%expr
      function
      | ppx____parser____stream____ -> (
          match [%e peek] with
          | Some 1 -> (
              let () = [%e junk] in
              match [%e peek] with
              | Some 2 ->
                  let () = [%e junk] in
                  3
              | _ -> [%e raise_err_exn])
          | _ -> (
              match
                try Some (some_call ppx____parser____stream____)
                with Stream.Failure -> None
              with
              | Some c -> (
                  match [%e peek] with
                  | Some 4 ->
                      let () = [%e junk] in
                      c + 5
                  | _ -> [%e raise_err_exn])
              | _ -> (
                  match [%e peek] with
                  | Some 6 -> (
                      let () = [%e junk] in
                      match [%e peek] with
                      | Some 7 ->
                          let () = [%e junk] in
                          8
                      | _ -> [%e raise_err_exn])
                  | _ -> (
                      match
                        try Some (some_call ppx____parser____stream____)
                        with Stream.Failure -> None
                      with
                      | Some a ->
                          let b =
                            try some_call ppx____parser____stream____
                            with Stream.Failure -> [%e raise_err_exn]
                          in
                          a + b + 9
                      | _ -> [%e raise_fail_exn]))))]
  in
  check_eq ~expected ~actual "peek_call_alternate"

let tests =
  let open Alcotest in
  [
    test_case "alias" `Quick test_alias_parser;
    test_case "call_alias" `Quick test_call_alias_parser;
    test_case "let_ext" `Quick test_let_ext_parser;
    test_case "anom_parser" `Quick test_anom_parser_parser;
    test_case "let_parser" `Quick test_let_parser_parser;
    test_case "multi_bind" `Quick test_multi_bind_parser;
    test_case "multi_bind_w_args" `Quick test_multi_bind_w_args_parser;
    test_case "parser_calls" `Quick test_parser_calls_parser;
    test_case "or_pat_alias" `Quick test_or_pat_alias_parser;
    test_case "bind_pats" `Quick test_bind_pats_parser;
    test_case "peek_call_alternate" `Quick test_peek_call_alternate_parser;
  ]
