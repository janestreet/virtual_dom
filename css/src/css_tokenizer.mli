(** A css3 tokenizer.

    See section 4.1.1 on this page: https://www.w3.org/TR/CSS21/syndata.html

    Differences to the standard: {v
      - This does not implement BAD_STRING, BAD_URI, BAD_COMMENT
        That is we are not implementing any support for robustness in the face
        of bad input.  Rationale is we rather learn that we wrote invalid CSS.
      - We didn't do anything to validate the unicode support.
    v}

    Also we generally only implement what we need to parse declaration lists.
    Not everything needed to parse complete css style sheets (e.g. cdo, cdc).
*)
open! Core_kernel

type t

val create : string -> t

module Token : sig
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
  [@@deriving sexp]

  val equal : t -> t -> bool
end

val current : t -> Token.t

(** Start and len of the current token.  The eof token starts at String.length and has
    size 0. *)
val slice : t -> int * int

(** The textual representation of the current token. Note that it is exactly as given
    in the source. *)
val current_text : t -> string

(** source (create s) = s *)
val source : t -> string

(** Advance to the next token.  Idempotent if the current token is eof. *)
val next : t -> unit
