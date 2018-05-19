open Sexplib.Std

type t =
  | NodeList of t list
  | Comment of string
  | Text of string
  | Variable of vtag
  | RawVariable of vtag
  | Partial of ptag
  | Section of block
  | InvertedSection of block
and block = (vtag * t * vtag)
and vtag = (string * key * string)
and ptag = (string * ident * string)
and key = string list
and ident = string
[@@deriving sexp, show]

let to_string ast = ast |> sexp_of_t |> Sexplib.Sexp.to_string
let to_string_hum ast = ast |> sexp_of_t |> Sexplib.Sexp.to_string_hum

let of_string str = str |> Sexplib.Sexp.of_string |> t_of_sexp

exception BlockError of (string * block)

let block_error msg b = BlockError (msg, b)

let block b =
  let (t1, _, t2) = b in
  let (_, k1, _) = t1 in
  let (_, k2, _) = t2 in
  if k1 = k2 then b
  else raise (block_error "Mismatched keys" b)

let print_position ff lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.fprintf ff "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let error_message msg lexbuf =
  Format.asprintf "%a: %s\n" print_position lexbuf msg

exception SyntaxError of string

let syntax_error msg lexbuf =
  SyntaxError (error_message msg lexbuf)
