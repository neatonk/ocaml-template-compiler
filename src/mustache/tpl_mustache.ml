let of_string s = Mustache.of_string s

let to_string x = Mustache.to_string x

let%expect_test "roundtrip" =
  print_newline ();
  String.concat "\n"
    [
      "Hello {{name}}!";
      "Hello";
      "{{name}}!";
    ]
  |> of_string |> to_string
  |> print_endline;
  [%expect_exact {|
Hello {{name}}!
Hello
{{name}}!
|}]
