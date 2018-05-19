{

open Lexing
open Parser

type state_t =
  | Tok of (token * state_t)
  | Text
  | Comment
  | Tag
  | Done

exception Error of string

let syntax_error = Tpl_core.Ast.syntax_error

let error_bad_input lexbuf =
  syntax_error ("Unexpected char: " ^ (lexeme lexbuf)) lexbuf

let error_eof lexbuf =
  syntax_error "Unexpected EOF" lexbuf

let acc_with acc state lexbuf f =
  f ((lexeme lexbuf) :: acc) state lexbuf

let acc_string acc =
  String.concat "" (List.rev acc)

let finish_text = function
  | [] -> None | acc -> Some (TEXT (acc_string acc))

let cmt_open acc state =
  state := Comment;
  finish_text acc

let cmt_close acc state =
  state := Text;
  Some (COMMENT (acc_string acc))

let tag_open acc state tok =
  if acc = [] then begin
    state := Tag;
    Some tok
  end else begin
    state := Tok (tok, Tag);
    finish_text acc
  end

let tag_close state tok =
  state := Text;
  Some tok

}

let ws = [' ' '\t']*
let txt = [^ '{' '\r' '\n']*
let cmt = [^ '}' '\r' '\n']*
let nl = ('\r' | '\n' | "\r\n")
let ident = [^ '}' '\t' '\r' '\n' ' ']+

rule tag state = parse
  | ws       { Some (TAG_PAD (lexeme lexbuf)) }
  | ident    { Some (TAG_IDENT (lexeme lexbuf)) }
  | "}}}"    { tag_close state TAG_CLOSE_TRIPLE }
  | "}}"     { tag_close state TAG_CLOSE_DOUBLE }
  | _        { raise (error_bad_input lexbuf) }
  | eof      { raise (error_eof lexbuf) }

and comment acc state = parse
  | cmt nl { new_line lexbuf;
             acc_with acc state lexbuf comment }
  | cmt    { acc_with acc state lexbuf comment }
  | "}}"   { cmt_close acc state }
  | '}'    { acc_with acc state lexbuf comment }
  | _      { raise (error_bad_input lexbuf) }
  | eof    { raise (error_eof lexbuf) }


and text acc state = parse
  | txt nl { new_line lexbuf;
             acc_with acc state lexbuf text }
  | txt    { acc_with acc state lexbuf text }
  | "{{!"  { cmt_open acc state }
  | "{{#"  { tag_open acc state TAG_OPEN_BEGIN }
  | "{{^"  { tag_open acc state TAG_OPEN_BEGIN_INVERTED }
  | "{{/"  { tag_open acc state TAG_OPEN_END }
  | "{{>"  { tag_open acc state TAG_OPEN_PARTIAL }
  | "{{&"  { tag_open acc state TAG_OPEN_UNESCAPE }
  | "{{{"  { tag_open acc state TAG_OPEN_TRIPLE }
  | "{{"   { tag_open acc state TAG_OPEN_DOUBLE }
  | '{'    { acc_with acc state lexbuf text }
  | _      { raise (error_bad_input lexbuf) }
  | eof    { state := Done;
             finish_text acc }

{

let raw_token_opt state lexbuf =
  match !state with
  | Tok (t, s) -> state := s; Some t
  | Text       -> text [] state lexbuf
  | Comment    -> comment [] state lexbuf
  | Tag        -> tag state lexbuf
  | Done       -> Some EOF

let rec raw_token state lexbuf =
  match raw_token_opt state lexbuf with
  | None     -> raw_token state lexbuf
  | Some tok -> tok

let token state lexbuf =
  ((raw_token state lexbuf),
   lexbuf.Lexing.lex_start_p,
   lexbuf.Lexing.lex_curr_p)

let lexfun lexbuf =
  let state = ref Text in
  fun () -> token state lexbuf

}
