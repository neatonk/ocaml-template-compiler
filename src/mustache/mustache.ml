let of_string s =
  let lexbuf = Lexing.from_string s in
  let lex =
    let open Lexer in
    let state = initial_state () in
    fun () -> token state lexbuf
  in
  MenhirLib.Convert.Simplified.(
    traditional2revised Parser.parse lex
  )

let to_string m =
  let buf = Buffer.create 0 in
  let ff = Format.formatter_of_buffer buf in
  Printer.pp ff m;
  Format.pp_print_flush ff ();
  Buffer.contents buf
