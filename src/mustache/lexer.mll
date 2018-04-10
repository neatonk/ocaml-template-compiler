{

open Lexing
open Parser
open Types

type state_t =
  | Tag
  | Content

}

let text = [^ '{' '\r' '\n']*
let newline = ('\r' | '\n' | "\r\n")
let key = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+
let key_sep = ['.']

rule tag state = parse
  | key      { TAG_KEY (lexeme lexbuf) }
  | key_sep  { TAG_KEY_SEP (lexeme lexbuf) }
  | "}}"     { state := Content;
               TAG_CLOSE (lexeme lexbuf) }

and content state = parse
  | text newline { new_line lexbuf;
                   TEXT (lexeme lexbuf) }
  | text         { TEXT (lexeme lexbuf) }
  | "{{"         { state := Tag;
                   TAG_OPEN (lexeme lexbuf) }
  | eof          { EOF }

{

  let initial_state () = ref Content

  let raw_token state lexbuf =
    match !state with
    | Tag -> tag state lexbuf
    | Content -> content state lexbuf

  let token state lexbuf =
    ((raw_token state lexbuf),
     lexbuf.Lexing.lex_start_p,
     lexbuf.Lexing.lex_curr_p)
}
