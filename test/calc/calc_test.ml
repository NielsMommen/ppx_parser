module Calc1 = struct
  let left_assoc op elem =
    let rec op_elem x =
      function%parser
      | [ (op as t); (elem as y); [%l r = op_elem (t x y)] ] -> r | [] -> x
    in
    function%parser [ (elem as x); [%l r = op_elem x] ] -> r

  let right_assoc op elem =
    let rec op_elem x =
      function%parser
      | [ (op as t); (elem as y); [%l r = op_elem y] ] -> t x r | [] -> x
    in
    function%parser [ (elem as x); [%l r = op_elem x] ] -> r

  let parse =
    List.fold_right
      (fun op elem -> op elem)
      [
        left_assoc (function%parser [ '+' ] -> fun x y -> x +. y);
        left_assoc (function%parser [ '*' ] -> fun x y -> x *. y);
        right_assoc (function%parser [ '^' ] -> fun x y -> x ** y);
      ]
      (function%parser
      | [ ('0' .. '9' as c) ] -> float (Char.code c - Char.code '0'))
end

module Calc2 = struct
  type tok = PLUS | MINUS | TIMES | DIV | LPAR | RPAR | INT of int

  type ast =
    | Int of int
    | Plus of ast * ast
    | Minus of ast * ast
    | Mult of ast * ast
    | Div of ast * ast
  [@@deriving show, eq]

  let rec spaces =
    function%parser [ (' ' | '\t' | '\n'); (spaces as _s) ] -> () | [] -> ()

  let int_of_digit = function
    | '0' .. '9' as c -> int_of_char c - int_of_char '0'
    | _ -> raise (Failure "not a digit")

  let rec integer n =
    function%parser
    | [ ('0' .. '9' as c); [%l r = integer ((10 * n) + int_of_digit c)] ] -> r
    | [] -> n

  let lexer s =
    let p =
      function%parser
      | [ '('; (spaces as _s) ] -> Some LPAR
      | [ ')'; (spaces as _s) ] -> Some RPAR
      | [ '+'; (spaces as _s) ] -> Some PLUS
      | [ '-'; (spaces as _s) ] -> Some MINUS
      | [ '*'; (spaces as _s) ] -> Some TIMES
      | [ '/'; (spaces as _s) ] -> Some DIV
      | [ ('0' .. '9' as c); [%l n = integer (int_of_digit c)]; (spaces as _s) ]
        ->
          Some (INT n)
      | [] -> None
    in
    Stream.from (fun _ -> p s)

  let addop =
    function%parser
    | [ PLUS ] -> ( function x, y -> Plus (x, y))
    | [ MINUS ] -> ( function x, y -> Minus (x, y))

  let multop =
    function%parser
    | [ TIMES ] -> ( function x, y -> Mult (x, y))
    | [ DIV ] -> ( function x, y -> Div (x, y))

  let left_assoc op term =
    let rec rest e1 =
      function%parser
      | [ (op as f); (term as e2); [%l e = rest (f (e1, e2))] ] -> e | [] -> e1
    in
    function%parser [ (term as e1); [%l e2 = rest e1] ] -> e2

  let rec expr str = left_assoc addop mult str
  and mult str = left_assoc multop atom str

  and atom =
    function%parser [ INT n ] -> Int n | [ LPAR; (expr as e); RPAR ] -> e
end

let calc1 s =
  let open Calc1 in
  s |> Stream.of_string |> parse

let calc2 s =
  let open Calc2 in
  s |> Stream.of_string |> lexer |> expr

let mk_test testble calc (input, output) =
  let test () =
    Alcotest.(check' testble) ~msg:input ~expected:output ~actual:(calc input)
  in
  Alcotest.test_case input `Quick test

let tests1 =
  [
    ("1+8+3", 1. +. 8. +. 3.);
    ("2*3*6", 2. *. 3. *. 6.);
    ("1+4*3+6+2+5*2", 1. +. (4. *. 3.) +. 6. +. 2. +. (5. *. 2.));
    ("1+3^2*2^3", 1. +. ((3. ** 2.) *. (2. ** 3.)));
  ]
  |> List.map (mk_test (Alcotest.float epsilon_float) calc1)

let tests2 =
  let open Calc2 in
  let testble =
    Alcotest.testable
      (fun fmt a -> show_ast a |> Format.pp_print_string fmt)
      equal_ast
  in
  [
    ("1 + 2 - 5 + 3", Plus (Minus (Plus (Int 1, Int 2), Int 5), Int 3));
    ( "(1 + 2 + 3 * 4) - 567",
      Minus (Plus (Plus (Int 1, Int 2), Mult (Int 3, Int 4)), Int 567) );
    ( "1 + (3 * 2 + 1 + 5 / 2) * 8 - 3",
      Minus
        ( Plus
            ( Int 1,
              Mult
                ( Plus (Plus (Mult (Int 3, Int 2), Int 1), Div (Int 5, Int 2)),
                  Int 8 ) ),
          Int 3 ) );
  ]
  |> List.map (mk_test testble calc2)
