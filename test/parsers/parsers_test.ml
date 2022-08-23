(* (1) Ignore the result of a parser call when it is a simple function name. *)
let rec consume_spaces = 
  function%parser
  | [ ' ' | '\t' | '\n'; consume_spaces (* (1) *) ] -> ()
  | [ ] -> ()

let test_consume_spaces () =
  let strm = Stream.of_string("  \t  \n   rest  \t\n") in
  let () = consume_spaces strm in
  let left = ref [] in
  let () = Stream.iter (fun i -> left := i :: !left) strm in
  let actual = List.rev !left in
  let expected = ['r'; 'e'; 's'; 't'; ' '; ' '; '\t'; '\n'] in
  Alcotest.(check' (list char)) ~msg:"consume spaces" ~expected ~actual

(* (1) Bind the result of a parser call to a constant in a '%let' extension *)
let [@warning "-8"] bind_const =
  let expect el =
    function%parser
    | [ k ] when k = el -> true
    | [ ] -> false
  in
  function%parser
  | [ [%let true = expect 1]; [%let true = expect 2]] -> true
  | [ ] -> false

let test_bind_const () =
  let strm = Stream.of_list [1; 2] in
  let actual = bind_const strm in
  let expected = true in
  Alcotest.(check' bool) ~msg:"bind const" ~expected ~actual

(* (1) Bind the result of a parser call to a variable in a '%let' extension *)
let count_consumed_spaces =
  let rec count i =
    function%parser
    | [ ' ' | '\t' | '\n'; [%let i = count (i + 1)] (* (1) *) ] -> i
    | [ ] -> i
  in
  count 0

let test_count_consumed_spaces () =
  let strm = Stream.of_string "    " in
  let actual = count_consumed_spaces strm in
  let expected = 4 in
  Alcotest.(check' int) ~msg:"count consumed spaces" ~expected ~actual

(* (1) User parser call as first element to match in the stream. The parser can try to match the second list of elements
  in case of Stream.Failure because the strream did not change. *)
let consume_spaces_get_chars_codes =
  let parse_char = 
    function%parser
    | [ ' ' | '\t' | '\n' as c] -> Char.code c
  in
  let rec parse chars =
    function%parser
    | [ parse_char as c (* (1) *); [%let cs = parse (c :: chars)] ] -> cs
    | [ ] -> List.rev chars
  in
  parse []

(* (1) Use an anonymous parser inside a '%let' extension. *)
let consume_spaces_get_chars_codes_anom_parser =
  let rec parse chars =
    function%parser
    | [ 
        [%let c = 
          function%parser (* 1 *)
          | [ ' ' | '\t' | '\n' as c] -> Char.code c
        ]; 
        [%let cs = parse (c :: chars)] 
      ] -> cs
    | [ ] -> List.rev chars
  in
  parse []

(* (1) Use a 'let' binding inside a '%let' extension. *)
let consume_spaces_get_chars_codes_let_parser =
  let rec parse chars =
    function%parser
    | [ 
        [%let c = 
          let parse_char = (* 1 *)
            function%parser
            | [ ' ' | '\t' | '\n' as c] -> Char.code c
          in
          parse_char
        ]; 
        [%let cs = parse (c :: chars)] 
      ] -> cs
    | [ ] -> List.rev chars
  in
  parse []

let test_char_codes1, test_char_codes2, test_char_codes3 =
  let chars = [' '; '\t'; '\n'; '\n'; ' '] in
  let mk_strm () = Stream.of_list chars in
  let actual1 = consume_spaces_get_chars_codes (mk_strm ()) in
  let actual2 = consume_spaces_get_chars_codes_anom_parser (mk_strm ()) in
  let actual3 = consume_spaces_get_chars_codes_let_parser (mk_strm ()) in
  let expected = chars |> List.map Char.code in
  (fun () -> Alcotest.(check' (list int)) ~msg:"consume spaces get char codes" ~expected ~actual:actual1),
  (fun () -> Alcotest.(check' (list int)) ~msg:"consume spaces get char codes anonymous parser" ~expected ~actual:actual2),
  (fun () -> Alcotest.(check' (list int)) ~msg:"consume spaces get char codes let parser" ~expected ~actual:actual3)

(* (1) bind result of parser call to a tuple inside a '%let' extension. *)
let split_tuples =
  let rec split aa bb = 
    function%parser
    | [ a, b; [%let aa, bb (* 1 *) = split (a :: aa) (b :: bb)] ] -> aa, bb
    | [ ] -> List.rev aa, List.rev bb
  in
  split [] []

let test_split_tuples () =
  let lst1 = [1; 3; 5; 7] in
  let lst2 = [2; 4; 6; 8] in
  let strm = Stream.of_list (List.combine lst1 lst2) in
  let actual = split_tuples strm in
  let expected = lst1, lst2 in
  Alcotest.(check' (pair (list int) (list int))) ~msg:"split tuples" ~expected ~actual

type ('a, 'b) record_tuple = {
  rt_a: 'a;
  rt_b: 'b;
}
[@@deriving show, eq]

(* bind result of parsre call to a record inside a '%let' extension. *)
let split_rec_tuple_fields =
  let rec p r = function%parser
  | [ {rt_a; rt_b}; [%let {rt_a = a; rt_b = b} (* 1 *) = p {rt_a = rt_a :: r.rt_a; rt_b = rt_b :: r.rt_b}] ] -> {rt_a = b; rt_b = a}
  | [ ] -> {rt_a = List.rev r.rt_a; rt_b = List.rev r.rt_b}
  in
  p {rt_a = []; rt_b = []}

let test_split_rec_tuple_fields () =
  let lst1 = [1; 3; 5; 7] in
  let lst2 = [2; 4; 6; 8] in
  let rs = List.map2 (fun a b -> {rt_a = a; rt_b = b}) lst1 lst2 in
  let strm = Stream.of_list rs in
  let expected = {rt_a = lst1; rt_b = lst2} in
  let actual = split_rec_tuple_fields strm in
  let lst_formatter = Format.pp_print_list (fun fmt i -> Format.pp_print_int fmt i) in
  let testbl = Alcotest.testable (fun fmt r -> show_record_tuple lst_formatter lst_formatter r |> Format.pp_print_string fmt) (equal_record_tuple (List.equal Int.equal) (List.equal Int.equal)) in
  Alcotest.(check' testbl) ~msg:"split record tuple fields" ~actual ~expected

type lit = Int of int

let test_bindings () =
  let get_const_one _ = 1 in
  let get_tuple_one_two _ = 1, 2 in
  let get_rec_tuple_one_two _ = {rt_a = 1; rt_b = 2} in
  let get_some_one _ = Some 1 in
  let get_none _ = None in
  let get_int_lit_one _ = Int 1 in
  let get_arr _ = [| 1; 2; 3 |] in
  let get_lazy _ = lazy (1 + 2) in
  let [@warning "-8"] parser = 
    function%parser
    | [ [%let 1 = get_const_one]; [%let (a, 2) = get_tuple_one_two] ] -> a
    | [ 2; [%let {rt_b; rt_a} = get_rec_tuple_one_two]; [%let Some x = get_some_one] ] -> rt_b + rt_a + x
    | [ 3; [%let None = get_none]; [%let Int x = get_int_lit_one]; ] -> x
    | [ 4; [%let [| a; b; c |] = get_arr]; [%let lazy x = get_lazy] ] -> a + b + c + x
  in
  let strm = Stream.of_list [2; 3; 4] in
  let expected = [1; 4; 1; 9] in
  let actual = [parser strm; parser strm; parser strm; parser strm] in
  Alcotest.(check' (list int)) ~msg:"pattern bindings" ~expected ~actual

let tests = let open Alcotest in [
  test_case "consume_spaces" `Quick test_consume_spaces;
  test_case "bind_const" `Quick test_bind_const;
  test_case "count_consumed_spaces" `Quick test_count_consumed_spaces;
  test_case "consume_spaces_get_char_codes" `Quick test_char_codes1;
  test_case "consume_spaces_get_char_code_anonymous_parser" `Quick test_char_codes2;
  test_case "consume_spaces_get_char_code_let_parser" `Quick test_char_codes3;
  test_case "split_tuples" `Quick test_split_tuples;
  test_case "split_record_tuple_fields" `Quick test_split_rec_tuple_fields;
]