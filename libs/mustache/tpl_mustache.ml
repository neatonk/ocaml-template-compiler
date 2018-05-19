let parse lexbuf =
  let lex = Lexer.lexfun lexbuf in
  MenhirLib.Convert.Simplified.(
    traditional2revised Parser.parse lex
  )

let set_fname fname lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let of_string ?(fname="file") s =
  parse (Lexing.from_string s |> set_fname fname)

let of_channel ?(fname="file") ch =
  parse (Lexing.from_channel ch |> set_fname fname)

let to_string m =
  let buf = Buffer.create 0 in
  let ff = Format.formatter_of_buffer buf in
  Printer.pp ff m;
  Format.pp_print_flush ff ();
  Buffer.contents buf

let%expect_test "parse" =
  let fname = "test.mustache" in
  open_in fname
  |> of_channel ~fname
  |> (Tpl_core.Ast.pp Format.std_formatter);
  [%expect {|
    (Ast.NodeList
       [(Ast.Comment " This is a single-line simple comment "); (Ast.Text "\n");
         (Ast.Comment "\n\n        This is a multi-line\n        comment :p\n\n");
         (Ast.Text "\nHello "); (Ast.Variable ("", ["name"], ""));
         (Ast.Text "!\nHello\n"); (Ast.Variable ("", ["name"], ""));
         (Ast.Text "!\nThis is not a {bug} :)\nMy items:\n ");
         (Ast.Section
            ((" ", ["items"], ""),
             (Ast.NodeList [(Ast.Text "- "); (Ast.Variable ("", [], ""))]),
             ("", ["items"], "")));
         (Ast.Text "\n ");
         (Ast.InvertedSection
            (("", ["items"], " "), (Ast.NodeList [(Ast.Text "(EMPTY)")]),
             ("", ["items"], "")));
         (Ast.Text "\n"); (Ast.Partial (" ", "myPartial", "")); (Ast.Text "\n")]) |}]

let%expect_test "roundtrip" =
  print_newline ();
  let fname = "test.mustache" in
  open_in fname
  |> of_channel ~fname
  |> to_string
  |> print_string;
  [%expect_exact {|
{{! This is a single-line simple comment }}
{{!

        This is a multi-line
        comment :p

}}
Hello {{name}}!
Hello
{{name}}!
This is not a {bug} :)
My items:
 {{# items}}- {{.}}{{/items}}
 {{^items }}(EMPTY){{/items}}
{{> myPartial}}
|}]
