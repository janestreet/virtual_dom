open Core
open! Int.Replace_polymorphic_compare

module Token = struct
  type t =
    | Ident
    | Function
    | Atkeyword
    | Hash
    | String
    | Uri
    | Delim
    | Number
    | Percentage
    | Dimension
    | White_space
    | Colon
    | Semi_colon
    | Comma
    | Lbracket
    | Rbracket
    | Lparen
    | Rparen
    | Lcurly
    | Rcurly
    | Comment
    | Eof
    | Error
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]
end

type t =
  { s : string
  ; mutable off : int
  ; (* start of the current token *)
    mutable len : int
  ; (* length of the current token *)
    mutable current : Token.t
  }

type mark = Mark of int [@@unboxed]

let mark t = Mark t.len
let source t = t.s
let next_pos t = t.off + t.len
let is_next_eof t = next_pos t >= String.length t.s
let next_char t = t.s.[next_pos t]
let reset t (Mark v) = t.len <- v
let consume_1 t = t.len <- t.len + 1
let consume_n t n = t.len <- t.len + n

let one_char_token t tok =
  assert (t.len = 0);
  consume_1 t;
  t.current <- tok
;;

exception Error_happened

let error t =
  t.current <- Error;
  raise Error_happened
;;

let accept t f =
  if (not (is_next_eof t)) && f (next_char t)
  then (
    consume_1 t;
    true)
  else false
;;

let accept_char t ch = accept t (Char.equal ch)

let accept_string t str =
  if String.length str = 0
  then true
  else (
    let pos = next_pos t in
    let str_len = String.length str in
    if pos + str_len > String.length t.s
    then false
    else if String.is_substring_at t.s ~pos ~substring:str
    then (
      consume_n t str_len;
      true)
    else false)
;;

let expect t f = if accept t f then () else error t
let expect_char t ch = expect t (Char.equal ch)

let many t f =
  while (not (is_next_eof t)) && f (next_char t) do
    consume_1 t
  done
;;

let many1 t f =
  expect t f;
  many t f
;;

let plus_or_minus = function
  | '-' | '+' -> true
  | _ -> false
;;

let nmstart = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let nmchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '-' -> true
  | _ -> false
;;

let ident t =
  if accept_char t '-'
  then
    if accept_char t '-'
    then many t nmchar
    else (
      expect t nmstart;
      many t nmchar)
  else (
    expect t nmstart;
    many t nmchar)
;;

let ident_or_function t =
  ident t;
  if accept_char t '(' then t.current <- Function else t.current <- Ident
;;

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' | '\x0c' -> true
  | _ -> false
;;

let quoted_string' t ~quote =
  assert (Char.equal (next_char t) quote);
  consume_1 t;
  let rec loop () =
    many t (function
      | '\n' | '\r' | '\x0c' | '\\' -> false
      | c when Char.equal c quote -> false
      | _ -> true);
    if accept_char t '\\'
    then
      if accept_char t quote
      then loop ()
      else (
        match next_char t with
        | '\n' | '\x0c' ->
          consume_1 t;
          loop ()
        | '\r' ->
          consume_1 t;
          ignore (accept_char t '\n' : bool);
          loop ()
        | _c ->
          (* Handle the "escape" rule:
             {v
                unicode: \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
                escape : {unicode}|\\[^\n\r\f0-9a-f]
              v}
          *)
          consume_1 t;
          loop ())
  in
  loop ();
  expect_char t quote
;;

let start_num t =
  let cur = mark t in
  let b =
    ignore (accept t plus_or_minus : bool);
    ignore (accept_char t '.' : bool);
    accept t Char.is_digit
  in
  reset t cur;
  b
;;

let start_ident ~prefix t =
  let cur = mark t in
  let b =
    accept_string t prefix
    &&
    if accept_char t '-' then accept_char t '-' || accept t nmstart else accept t nmstart
  in
  reset t cur;
  b
;;

let quoted_string t ~quote =
  assert (t.len = 0);
  quoted_string' t ~quote;
  t.current <- String
;;

let exponential_part t =
  let mark = mark t in
  if accept t (function
    | 'e' | 'E' -> true
    | _ -> false)
  then
    if accept t plus_or_minus
    then many1 t Char.is_digit
    else if accept t Char.is_digit
    then many t Char.is_digit
    else
      (* Backtrack if there are no digits or signs after the 'e' because
         it is possible that the 'e' was actually a part of something else. For
         example, the 'e' could be part of an 'em' or 'en' dimension instead
         of beginning the exponential part of a scientific notation number.

         Example: 1.2e3 is scientific notation, but 1.2em is a number with a
         dimension. *)
      reset t mark
;;

let num t =
  ignore (accept t plus_or_minus : bool);
  if accept_char t '.'
  then many1 t Char.is_digit
  else (
    many1 t Char.is_digit;
    if accept_char t '.' then many1 t Char.is_digit);
  exponential_part t
;;

let number_or_percentage_or_dimension t =
  assert (t.len = 0);
  num t;
  if accept_char t '%'
  then t.current <- Percentage
  else if start_ident ~prefix:"" t
  then (
    ident t;
    t.current <- Dimension)
  else t.current <- Number
;;

let hash_or_delim t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) '#');
  consume_1 t;
  if accept t nmchar
  then (
    many1 t nmchar;
    t.current <- Hash)
  else t.current <- Delim
;;

let atkeyword t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) '@');
  consume_1 t;
  ident t;
  t.current <- Atkeyword
;;

let uri_or_ident_or_function t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) 'u');
  if accept_string t "url("
  then (
    let m = mark t in
    many t is_whitespace;
    if Char.equal (next_char t) '"' || Char.equal (next_char t) '\''
    then (
      reset t m;
      t.current <- Function)
    else (
      many t (function
        | ')' | ' ' -> false
        | '\'' | '"' | '(' -> error t
        | c when Char.is_print c -> true
        | _ -> error t);
      many t is_whitespace;
      expect_char t ')';
      t.current <- Uri))
  else ident_or_function t
;;

let comment_or_delim t =
  assert (t.len = 0);
  if accept_string t "/*"
  then (
    let rec loop () =
      many t (function
        | '*' -> false
        | _ -> true);
      if accept_string t "*/"
      then t.current <- Comment
      else if accept_char t '*'
      then loop ()
      else error t
    in
    loop ())
  else (
    expect_char t '/';
    t.current <- Delim)
;;

let next t =
  let new_off = next_pos t in
  t.off <- new_off;
  t.len <- 0;
  if is_next_eof t
  then t.current <- Eof
  else (
    match next_char t with
    | c when is_whitespace c ->
      many t is_whitespace;
      t.current <- White_space
    | ':' -> one_char_token t Colon
    | ';' -> one_char_token t Semi_colon
    | '{' -> one_char_token t Lcurly
    | '}' -> one_char_token t Rcurly
    | '[' -> one_char_token t Lbracket
    | ']' -> one_char_token t Rbracket
    | '(' -> one_char_token t Lparen
    | ')' -> one_char_token t Rparen
    | ',' -> one_char_token t Comma
    | ('\'' | '"') as quote -> quoted_string t ~quote
    | ('0' .. '9' | '.' | '+' | '-') when start_num t ->
      number_or_percentage_or_dimension t
    | '/' -> comment_or_delim t
    | '#' -> hash_or_delim t
    | '@' when start_ident ~prefix:"@" t -> atkeyword t
    | 'u' -> uri_or_ident_or_function t
    | _ ->
      if start_ident ~prefix:"" t
      then ident_or_function t
      else (
        consume_1 t;
        (* this is weird but the spec is weird *)
        t.current <- Delim))
;;

let next t =
  (* Make sure we are idempotent when we get into the error state *)
  if Token.equal t.current Error
  then ()
  else (
    try next t with
    | Error_happened -> ())
;;

let create s =
  (* current is dummy and will be set by call to next *)
  let t = { s; off = 0; len = 0; current = White_space } in
  next t;
  t
;;

let slice t = t.off, t.len
let current t = t.current

let current_text t =
  if Token.equal t.current Eof
  then ""
  else (
    let pos, len = slice t in
    String.sub t.s ~pos ~len)
;;

let to_list s =
  let t = create s in
  let rec loop acc =
    let start, len = slice t in
    let acc = (current t, start, len) :: acc in
    if Token.equal (current t) Eof || Token.equal (current t) Error
    then List.rev acc
    else (
      next t;
      loop acc)
  in
  loop []
;;

let%test_module "tests" =
  (module struct
    let test s =
      print_endline
        (Sexp.to_string_mach ([%sexp_of: (Token.t * int * int) list] (to_list s)))
    ;;

    let%expect_test "eof" =
      test "";
      [%expect {|((Eof 0 0))|}]
    ;;

    let%expect_test "simple_tokens" =
      test ")({}[];:";
      [%expect
        {| ((Rparen 0 1)(Lparen 1 1)(Lcurly 2 1)(Rcurly 3 1)(Lbracket 4 1)(Rbracket 5 1)(Semi_colon 6 1)(Colon 7 1)(Eof 8 0)) |}]
    ;;

    let%expect_test "ident" =
      test "-foo-bar: baz";
      test "-foo-bar(";
      test "@foo-bar";
      test "@-foo-bar";
      test "@--foo-bar";
      test "--var";
      test "RGBA";
      [%expect
        {|
    ((Ident 0 8)(Colon 8 1)(White_space 9 1)(Ident 10 3)(Eof 13 0))
    ((Function 0 9)(Eof 9 0))
    ((Atkeyword 0 8)(Eof 8 0))
    ((Atkeyword 0 9)(Eof 9 0))
    ((Atkeyword 0 10)(Eof 10 0))
    ((Ident 0 5)(Eof 5 0))
    ((Ident 0 4)(Eof 4 0)) |}]
    ;;

    let%expect_test "whitespace" =
      test "  ";
      [%expect {|((White_space 0 2)(Eof 2 0))|}]
    ;;

    let%expect_test "numbers" =
      test "margin: 0.5en";
      test "margin: 0.5em";
      test "margin: 0.5in";
      test "line-height: 3cm";
      test "line-height: 120%";
      test "grid: 0 1 foo";
      test "margin: 0.5-in";
      test "margin: .02e+20";
      test "margin: .02e-20";
      test "margin: .02E+20";
      test "margin: .02E-20";
      test "margin: .02E20";
      test "margin: .02e20";
      [%expect
        {|
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 5)(Eof 13 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 5)(Eof 13 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 5)(Eof 13 0))
    ((Ident 0 11)(Colon 11 1)(White_space 12 1)(Dimension 13 3)(Eof 16 0))
    ((Ident 0 11)(Colon 11 1)(White_space 12 1)(Percentage 13 4)(Eof 17 0))
    ((Ident 0 4)(Colon 4 1)(White_space 5 1)(Number 6 1)(White_space 7 1)(Number 8 1)(White_space 9 1)(Ident 10 3)(Eof 13 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 6)(Eof 14 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 7)(Eof 15 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 7)(Eof 15 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 7)(Eof 15 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 7)(Eof 15 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 6)(Eof 14 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Number 8 6)(Eof 14 0)) |}]
    ;;

    let%expect_test "badly_quoted" =
      test "'foo bar";
      test "'";
      test "'\\'";
      [%expect {|
    ((Error 0 8))
    ((Error 0 1))
    ((Error 0 3)) |}]
    ;;

    let%expect_test "quoted" =
      test {|'Foo '"Bar"';' '\'' "\""|};
      [%expect
        {| ((String 0 6)(String 6 5)(String 11 3)(White_space 14 1)(String 15 4)(White_space 19 1)(String 20 4)(Eof 24 0)) |}]
    ;;

    let%expect_test "colors" =
      test {|color: #f00|};
      test {|color: rgb(255,0,0)|};
      test {|color: rgb(255,0,0)|};
      [%expect
        {|
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Hash 7 4)(Eof 11 0))
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Function 7 4)(Number 11 3)(Comma 14 1)(Number 15 1)(Comma 16 1)(Number 17 1)(Rparen 18 1)(Eof 19 0))
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Function 7 4)(Number 11 3)(Comma 14 1)(Number 15 1)(Comma 16 1)(Number 17 1)(Rparen 18 1)(Eof 19 0)) |}]
    ;;

    let%expect_test "import" =
      test {|@import "foo.bar";|};
      [%expect
        {| ((Atkeyword 0 7)(White_space 7 1)(String 8 9)(Semi_colon 17 1)(Eof 18 0)) |}]
    ;;

    let%expect_test "red example" =
      test "red-->";
      [%expect {| ((Ident 0 5)(Delim 5 1)(Eof 6 0)) |}]
    ;;

    let%expect_test "url" =
      test {|url( "http://wwww.google.com")|};
      test {|url('http://wwww.google.com')|};
      test {|url('http://wwww.google.com' )|};
      test {|url(  http://wwww.google.com )|};
      test {|url(  http://wwww.google."com )|};
      [%expect
        {|
    ((Function 0 4)(White_space 4 1)(String 5 24)(Rparen 29 1)(Eof 30 0))
    ((Function 0 4)(String 4 24)(Rparen 28 1)(Eof 29 0))
    ((Function 0 4)(String 4 24)(White_space 28 1)(Rparen 29 1)(Eof 30 0))
    ((Uri 0 30)(Eof 30 0))
    ((Error 0 25)) |}]
    ;;

    let%expect_test "escape" =
      test {|"test\19abf2\2"|};
      test {|"\010\xFFa\o123\n\\\u{12345}aa🐪🐪🐪🐪🐪"|};
      test {|"← ↑ → ↓ ↔ ↕ ⇪ ↹ ⬈ ↘ ⟾ ↶"|};
      [%expect
        {|
    ((String 0 15)(Eof 15 0))
    ((String 0 51)(Eof 51 0))
    ((String 0 49)(Eof 49 0)) |}]
    ;;
  end)
;;
