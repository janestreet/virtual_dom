open Core_kernel
open! Int_replace_polymorphic_compare

module Token = struct
  type t =
    | Ident
    | Atkeyword
    | String
    | Hash
    | Number
    | Percentage
    | Dimension
    | Uri
    | Lcurly
    | Rcurly
    | Lbracket
    | Rbracket
    | Lparen
    | Rparen
    | Colon
    | Semi
    | White_space
    | Comment
    | Function
    | Eof
    | Delim
    | Error
  [@@deriving sexp, compare]

  let equal = [%compare.equal : t]
end


type t = {
  s : string;
  mutable off : int; (* start of the current token *)
  mutable len : int; (* length of the current token *)
  mutable current : Token.t
}

let source t = t.s

let next_pos t = t.off + t.len

let is_next_eof t = next_pos t >= String.length t.s

let next_char t = t.s.[next_pos t]

let consume_1 t = t.len <- t.len + 1
let consume_n t n = t.len <- t.len + n

let one_char_token t tok =
  assert (t.len = 0);
  consume_1 t;
  t.current <- tok
;;

exception Error_happened

let error t = t.current <- Error; raise Error_happened

let accept t f =
  if not (is_next_eof t) && f (next_char t) then (consume_1 t; true) else false
;;

let accept_char t ch = accept t (Char.equal ch)

let accept_string t str =
  let pos = next_pos t in
  let str_len = String.length str in
  if pos + str_len > String.length t.s then
    false
  else if String.is_substring_at t.s ~pos ~substring:str then
    (consume_n t str_len; true)
  else
    false
;;

let expect t f =
  if accept t f then ()
  else (error t)
;;

let expect_char t ch = expect t (Char.equal ch)

let many t f =
  while not (is_next_eof t) && f (next_char t) do
    consume_1 t
  done
;;

let many1 t f =
  expect t f;
  many t f
;;

let nmstart c =
  Char.is_lowercase c || Char.equal c '_'
;;

let nmchar c =
  Char.is_lowercase c || Char.is_digit c || Char.equal c '_' || Char.equal c '-'
;;

let ident t =
  expect t nmstart;
  many t nmchar

let ident_or_function t =
  ident t;
  if accept_char t '(' then
    (t.current <- Function)
  else
    (t.current <- Ident)
;;

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' | '\x0c' -> true
  | _ -> false

let quoted_string' t ~quote =
  assert (Char.equal (next_char t) quote);
  consume_1 t;
  let rec loop () =
    many t (function
      | '\n' | '\r' | '\x0c' | '\\' -> false
      | c when Char.equal c quote -> false
      | _ -> true);
    if accept_char t '\\' then begin
      if accept_char t quote then (loop ())
      else begin
        match next_char t with
        | '\n' | '\x0c' -> consume_1 t; loop ()
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
          loop ()
      end
    end
  in
  loop ();
  expect_char t quote
;;

let quoted_string t ~quote =
  assert (t.len = 0);
  quoted_string' t ~quote;
  t.current <- String;
;;

let num t =
  if accept_char t '.' then begin
    many1 t Char.is_digit
  end else begin
    many1 t Char.is_digit;
    if accept_char t '.' then (many1 t Char.is_digit)
  end
;;

let number_or_percentage_or_dimension t =
  assert (t.len = 0);
  num t;
  if accept_char t '%' then begin
    t.current <- Percentage
  end else if accept_char t '-' then begin
    ident t;
    t.current <- Dimension
  end else if accept t nmstart then begin
    many t nmchar;
    t.current <- Dimension
  end else begin
    t.current <- Number
  end
;;

let hash t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) '#');
  consume_1 t;
  many1 t nmchar;
  t.current <- Hash
;;

let atkeyword t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) '@');
  consume_1 t;
  ignore (accept_char t '-' : bool);
  ident t;
  t.current <- Atkeyword
;;

let uri_or_ident_or_function t =
  assert (t.len = 0);
  assert (Char.equal (next_char t) 'u');
  if accept_string t "url(" then begin
    many t is_whitespace;
    begin match next_char t with
    | ('\'' | '"' as quote) -> quoted_string' t ~quote
    | _ -> error t
    end;
    many t is_whitespace;
    expect_char t ')';
    t.current <- Uri
  end else
    (ident_or_function t)
;;

let comment_or_delim t =
  assert (t.len = 0);
  if accept_string t "/*" then
    (let rec loop () =
       many t (function
         | '*' -> false
         | _ -> true);
       if accept_string t "*/" then
         (t.current <- Comment)
       else if accept_char t '*' then
         (loop ())
       else
         (error t)
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
  if is_next_eof t then (t.current <- Eof)
  else begin
    match next_char t with
    | c when is_whitespace c ->
      many t is_whitespace;
      t.current <- White_space
    | ':' ->
      one_char_token t Colon
    | ';' ->
      one_char_token t Semi
    | '{' ->
      one_char_token t Lcurly
    | '}' ->
      one_char_token t Rcurly
    | '[' ->
      one_char_token t Lbracket
    | ']' ->
      one_char_token t Rbracket
    | '(' ->
      one_char_token t Lparen
    | ')' ->
      one_char_token t Rparen
    | ('\''| '"') as quote ->
      quoted_string t ~quote
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' ->
      number_or_percentage_or_dimension t
    | '/' ->
      comment_or_delim t
    | '#' -> hash t
    | '@' -> atkeyword t
    | 'u' -> uri_or_ident_or_function t
    | '-' ->
      consume_1 t;
      if not (is_next_eof t) && nmstart (next_char t) then
        (ident_or_function t)
      else (t.current <- Delim)
    | ch ->
      if nmstart ch then (ident_or_function t)
      else (
        consume_1 t;
        (* this is weird but the spec is weird *)
        t.current <- Delim
      )
  end
;;

let next t =
  (* Make sure we are idempotent when we get into the error state *)
  if Token.equal t.current Error then ()
  else
    (try
       next t
     with Error_happened -> ())
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
  if Token.equal t.current Eof then ""
  else (
    let pos, len = slice t in
    String.sub t.s ~pos ~len)
;;

let to_list s =
  let t = create s in
  let rec loop acc =
    let start, len = slice t in
    let acc = (current t, start, len) :: acc in
    if Token.equal (current t) Eof || Token.equal (current t) Error then (List.rev acc)
    else (next t; loop acc)
  in
  loop []
;;

let test s =
  print_endline (Sexp.to_string_mach ([%sexp_of: (Token.t * int * int) list] (to_list s)))
;;

let%expect_test "eof" =
  test "";
  [%expect {|((Eof 0 0))|}]
;;

let%expect_test "simple_tokens" =
  test ")({}[];:";
  [%expect {| ((Rparen 0 1)(Lparen 1 1)(Lcurly 2 1)(Rcurly 3 1)(Lbracket 4 1)(Rbracket 5 1)(Semi 6 1)(Colon 7 1)(Eof 8 0)) |}]
;;

let%expect_test "ident" =
  test "-foo-bar: baz";
  test "-foo-bar(";
  test "@foo-bar";
  test "@-foo-bar";
  [%expect {|
    ((Ident 0 8)(Colon 8 1)(White_space 9 1)(Ident 10 3)(Eof 13 0))
    ((Function 0 9)(Eof 9 0))
    ((Atkeyword 0 8)(Eof 8 0))
    ((Atkeyword 0 9)(Eof 9 0)) |}]
;;

let%expect_test "whitespace" =
  test "  ";
  [%expect {|((White_space 0 2)(Eof 2 0))|}]
;;

let%expect_test "numbers" =
  test "margin: 0.5in";
  test "line-height: 3cm";
  test "line-height: 120%";
  test "grid: 0 1 foo";
  test "margin: 0.5-in";
  [%expect {|
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 5)(Eof 13 0))
    ((Ident 0 11)(Colon 11 1)(White_space 12 1)(Dimension 13 3)(Eof 16 0))
    ((Ident 0 11)(Colon 11 1)(White_space 12 1)(Percentage 13 4)(Eof 17 0))
    ((Ident 0 4)(Colon 4 1)(White_space 5 1)(Number 6 1)(White_space 7 1)(Number 8 1)(White_space 9 1)(Ident 10 3)(Eof 13 0))
    ((Ident 0 6)(Colon 6 1)(White_space 7 1)(Dimension 8 6)(Eof 14 0)) |}]
;;

let%expect_test "badly_quoted" =
  test "'foo bar";
  test "'";
  test "'\\'";
  [%expect{|
    ((Error 0 8))
    ((Error 0 1))
    ((Error 0 3)) |}]
;;

let%expect_test "quoted" =
  test {|'Foo '"Bar"';' '\'' "\""|};
  [%expect {| ((String 0 6)(String 6 5)(String 11 3)(White_space 14 1)(String 15 4)(White_space 19 1)(String 20 4)(Eof 24 0)) |}]
;;

let%expect_test "colors" =
  test {|color: #f00|};
  test {|color: rgb(255,0,0)|};
  test {|color: rgb(255,0,0)|};
  [%expect {|
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Hash 7 4)(Eof 11 0))
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Function 7 4)(Number 11 3)(Delim 14 1)(Number 15 1)(Delim 16 1)(Number 17 1)(Rparen 18 1)(Eof 19 0))
    ((Ident 0 5)(Colon 5 1)(White_space 6 1)(Function 7 4)(Number 11 3)(Delim 14 1)(Number 15 1)(Delim 16 1)(Number 17 1)(Rparen 18 1)(Eof 19 0)) |}]
;;

let%expect_test "import" =
  test {|@import "foo.bar";|};
  [%expect {| ((Atkeyword 0 7)(White_space 7 1)(String 8 9)(Semi 17 1)(Eof 18 0)) |}]
;;

let%expect_test "red example" =
  test "red-->";
  [%expect {| ((Ident 0 5)(Delim 5 1)(Eof 6 0)) |}]
;;

let%expect_test "url" =
  test {|url( "http://wwww.google.com")|};
  test {|url('http://wwww.google.com')|};
  test {|url('http://wwww.google.com' )|};
  [%expect {|
    ((Uri 0 30)(Eof 30 0))
    ((Uri 0 29)(Eof 29 0))
    ((Uri 0 30)(Eof 30 0)) |}]
;;

let%expect_test "escape" =
  test {|"test\19abf2\2"|};
  test {|"\010\xFFa\o123\n\\\u{12345}aa🐪🐪🐪🐪🐪"|};
  test {|"← ↑ → ↓ ↔ ↕ ⇪ ↹ ⬈ ↘ ⟾ ↶"|};
  [%expect {|
    ((String 0 15)(Eof 15 0))
    ((String 0 51)(Eof 51 0))
    ((String 0 49)(Eof 49 0)) |}]
