open Types
open Types.Mustache

let pp_text ff x = Format.pp_print_string ff x

let pp_tag_key ff = function
  | [] ->
     Format.fprintf ff "."
  | x :: xs ->
     Format.fprintf ff "%s" x;
     List.iter (Format.fprintf ff ".%s") xs

let pp_variable ff v =
  Format.fprintf ff "{{%a}}" pp_tag_key v

let rec pp ff = function
  | Text x -> pp_text ff x
  | Variable v -> pp_variable ff v
  | List xs -> List.iter (pp ff) xs
