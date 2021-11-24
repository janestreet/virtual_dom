(* Recursive descent parsers.  A parser returns false if based on a
   single token lookahead it decides that the given text can not be
   parsed.  Any other parse errors are handled by raising exceptions.

   Some parsers return unit because we only call them when a failure
   to parse implies a parse error (and not that some parser higher up
   in the call chain should try to parse something else).

   For simplicity the parsers themselves just validate and don't
   produce any values.  That leads to a few unelegant constructs
   (primarily in declaration), but means we otherwise have rather
   simple code that also allocates very little.
*)

open Core
open! Int.Replace_polymorphic_compare

let rec next ct =
  Css_tokenizer.next ct;
  if Css_tokenizer.(Token.equal (current ct) Comment) then next ct else ()
;;

let skip_white_space ct =
  while Css_tokenizer.(Token.equal (current ct) White_space) do
    next ct
  done
;;

let accept ct expected =
  let got = Css_tokenizer.current ct in
  if Css_tokenizer.Token.equal got expected
  then (
    next ct;
    true)
  else false
;;

let expect ct expected =
  let got = Css_tokenizer.current ct in
  if Css_tokenizer.Token.equal got expected
  then next ct
  else
    raise_s
      [%message
        "Unexpected token"
          (expected : Css_tokenizer.Token.t)
          (got : Css_tokenizer.Token.t)]
;;

let rec many (ct : Css_tokenizer.t) f = if f ct then many ct f else ()

let many1 (ct : Css_tokenizer.t) f =
  if f ct
  then (
    many ct f;
    true)
  else false
;;

let rec any ct : bool =
  let res =
    match Css_tokenizer.current ct with
    | Ident | Number | Percentage | Dimension | String | Uri | Delim | Hash | Comma ->
      next ct;
      true
    | Function ->
      next ct;
      skip_white_space ct;
      many ct any;
      expect ct Rparen;
      true
    | Lparen ->
      next ct;
      skip_white_space ct;
      expect_any ct;
      expect ct Rparen;
      true
    | Lbracket ->
      next ct;
      skip_white_space ct;
      expect_any ct;
      expect ct Rbracket;
      true
    | Rcurly | Rparen | Rbracket -> false
    | Lcurly -> false
    | Atkeyword | Colon | Semi_colon -> false
    | Comment | White_space | Eof | Error -> false
  in
  if res then skip_white_space ct else ();
  res

and expect_any ct = if any ct then () else raise_s [%message "Expected <any>"]

and value0 ct =
  any ct
  || block ct
  ||
  if accept ct Atkeyword
  then (
    skip_white_space ct;
    true)
  else false

and value ct = many1 ct value0

and block ct : bool =
  if accept ct Lcurly
  then (
    skip_white_space ct;
    many ct (fun ct ->
      value0 ct
      ||
      if accept ct Semi_colon
      then (
        skip_white_space ct;
        true)
      else false);
    expect ct Rcurly;
    skip_white_space ct;
    true)
  else false

and expect_value ct = if value ct then () else raise_s [%message "Expected <value>"]

let declaration ct =
  let ident_start, ident_len = Css_tokenizer.slice ct in
  if accept ct Ident
  then (
    skip_white_space ct;
    expect ct Colon;
    skip_white_space ct;
    let value_start = Css_tokenizer.slice ct |> fst in
    expect_value ct;
    let next_token_start = Css_tokenizer.slice ct |> fst in
    let source = Css_tokenizer.source ct in
    Some
      ( String.sub source ~pos:ident_start ~len:ident_len
      , String.rstrip
          (String.sub source ~pos:value_start ~len:(next_token_start - value_start)) ))
  else None
;;

let expect_declaration ct =
  match declaration ct with
  | Some (field, value) -> field, value
  | None -> raise_s [%message "Expected <declaration>"]
;;

(* As per: https://www.w3.org/TR/css-style-attr/
   declaration-list
   : S* declaration? [ ';' S* declaration? ]*
   ;
*)
let expect_declaration_list ct =
  let res = ref [] in
  let add kv =
    match kv with
    | None -> ()
    | Some (k, v) -> res := (k, v) :: !res
  in
  skip_white_space ct;
  add (declaration ct);
  many ct (fun ct ->
    if accept ct Semi_colon
    then (
      skip_white_space ct;
      add (declaration ct);
      true)
    else false);
  List.rev !res
;;

let parse parser_f s =
  let ct = Css_tokenizer.create s in
  while Css_tokenizer.(Token.equal (current ct) Comment) do
    Css_tokenizer.next ct
  done;
  Or_error.try_with (fun () ->
    let res = parser_f ct in
    expect ct Eof;
    res)
;;

let print_tokens s =
  let ct = Css_tokenizer.create s in
  while Css_tokenizer.(not (Token.equal (current ct) Eof)) do
    print_s (Css_tokenizer.Token.sexp_of_t (Css_tokenizer.current ct));
    Css_tokenizer.next ct
  done
;;

let validate_value = parse expect_value
let parse_declaration_list s = parse expect_declaration_list s

let test_parser p sexp_of_arg s =
  let r = parse p s in
  printf !"%s --> %{sexp:arg Or_error.t}\n" s r
;;

let%test_module "tests" =
  (module struct
    let%expect_test "" =
      let value =
        "0 4px 8px 0 RGBA(var(--js-text-color-rgb), 0.12), 0 2px 4px 0 \
         RGBA(var(--js-text-color-rgb), 0.08)"
      in
      print_tokens value;
      [%expect
        {|
    Number
    White_space
    Dimension
    White_space
    Dimension
    White_space
    Number
    White_space
    Function
    Function
    Ident
    Rparen
    Comma
    White_space
    Number
    Rparen
    Comma
    White_space
    Number
    White_space
    Dimension
    White_space
    Dimension
    White_space
    Number
    White_space
    Function
    Function
    Ident
    Rparen
    Comma
    White_space
    Number
    Rparen |}];
      print_s [%message (validate_value value : unit Or_error.t)];
      [%expect {|
    ("validate_value value" (Ok ())) |}]
    ;;

    let%expect_test "values" =
      let test = test_parser expect_value Unit.sexp_of_t in
      test "x";
      test "3";
      test "3in";
      test "3%";
      test "#fff";
      test "1 0 auto";
      test "'Hello World'";
      test "rgb(0,0,0)";
      [%expect
        {|
      x --> (Ok ())
      3 --> (Ok ())
      3in --> (Ok ())
      3% --> (Ok ())
      #fff --> (Ok ())
      1 0 auto --> (Ok ())
      'Hello World' --> (Ok ())
      rgb(0,0,0) --> (Ok ()) |}]
    ;;

    let%expect_test "declaration" =
      let test = test_parser expect_declaration [%sexp_of: string * string] in
      test "flex: 1 0 auto";
      test "content: 'Hello World'";
      test "content: foo;";
      (* Semi's are handled in declaration list *)
      test "content: bar ";
      (* but whitespace is handled in declaration (any really) *)
      [%expect
        {|
      flex: 1 0 auto --> (Ok (flex "1 0 auto"))
      content: 'Hello World' --> (Ok (content "'Hello World'"))
      content: foo; --> (Error ("Unexpected token" (expected Eof) (got Semi_colon)))
      content: bar  --> (Ok (content bar)) |}]
    ;;

    let%expect_test "unicode" =
      let test = test_parser expect_declaration [%sexp_of: string * string] in
      test "content: '← ↑ → ↓ ↔ ↕ ⇪ ↹ ⬈ ↘ ⟾ ↶'";
      print_endline (Sexp.to_string (Sexp.Atom "← ↑ → ↓ ↔ ↕ ⇪ ↹ ⬈ ↘ ⟾ ↶"));
      [%expect
        {|
    content: '← ↑ → ↓ ↔ ↕ ⇪ ↹ ⬈ ↘ ⟾ ↶' --> (Ok
     (content
      "'\226\134\144 \226\134\145 \226\134\146 \226\134\147 \226\134\148 \226\134\149 \226\135\170 \226\134\185 \226\172\136 \226\134\152 \226\159\190 \226\134\182'"))
    "\226\134\144 \226\134\145 \226\134\146 \226\134\147 \226\134\148 \226\134\149 \226\135\170 \226\134\185 \226\172\136 \226\134\152 \226\159\190 \226\134\182" |}]
    ;;

    let%expect_test "declaration list" =
      let test = test_parser expect_declaration_list [%sexp_of: (string * string) list] in
      test "flex: 1 0 auto";
      test "flex: 1 0 auto;";
      test
        "background: #5d9ab2 url(\"img_tree.png\") no-repeat top left;margin-left: 200px";
      test ";;;;;";
      test "flex: 1 0 auto ;; other : sa ";
      [%expect
        {|
    flex: 1 0 auto --> (Ok ((flex "1 0 auto")))
    flex: 1 0 auto; --> (Ok ((flex "1 0 auto")))
    background: #5d9ab2 url("img_tree.png") no-repeat top left;margin-left: 200px --> (Ok
     ((background "#5d9ab2 url(\"img_tree.png\") no-repeat top left")
      (margin-left 200px)))
    ;;;;; --> (Ok ())
    flex: 1 0 auto ;; other : sa  --> (Ok ((flex "1 0 auto") (other sa))) |}]
    ;;
  end)
;;
