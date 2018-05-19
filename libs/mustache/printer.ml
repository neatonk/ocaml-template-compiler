open Tpl_core.Ast

let pp_text ff x = Format.pp_print_string ff x

let pp_tag_key ff = function
  | [] ->
     Format.fprintf ff "."
  | x :: xs ->
     Format.fprintf ff "%s" x;
     List.iter (Format.fprintf ff ".%s") xs

let pp_vtag ff (a, b, c) tag_open =
  List.iter (Format.fprintf ff "%s") [ tag_open; a];
  pp_tag_key ff b;
  List.iter (Format.fprintf ff "%s") [ c; "}}" ]

let pp_ptag ff (a, b, c) tag_open =
  List.iter (Format.fprintf ff "%s")
    [ tag_open; a; b; c; "}}" ]

let pp_comment ff x =
  Format.fprintf ff "%s" "{{!";
  pp_text ff x;
  Format.fprintf ff "%s" "}}"

let rec pp_block ff (t1, xs, t2) tag_open =
  pp_vtag ff t1 tag_open;
  pp ff xs;
  pp_vtag ff t2 "{{/"
and pp ff = function
  | NodeList xs        -> List.iter (pp ff) xs
  | Comment x          -> pp_comment  ff x
  | Text x             -> pp_text     ff x
  | Variable t         -> pp_vtag     ff t "{{"
  | RawVariable t      -> pp_vtag     ff t "{{&"
  | Partial t          -> pp_ptag     ff t "{{>"
  | Section b          -> pp_block    ff b "{{#"
  | InvertedSection b  -> pp_block    ff b "{{^"
