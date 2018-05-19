%{
    open Tpl_core.Ast
%}

%token EOF
%token TAG_CLOSE_DOUBLE
%token TAG_CLOSE_TRIPLE
%token TAG_OPEN_BEGIN
%token TAG_OPEN_BEGIN_INVERTED
%token TAG_OPEN_DOUBLE
%token TAG_OPEN_END
%token TAG_OPEN_PARTIAL
%token TAG_OPEN_TRIPLE
%token TAG_OPEN_UNESCAPE
%token <string> COMMENT
%token <string> TAG_IDENT
%token <string> TAG_PAD
%token <string> TEXT

%start <Tpl_core.Ast.t> parse

%%

triple(X, Y, Z):
  | x = X; y = Y; z = Z { (x, y, z) }

tag_pad:
  | /* nothing */ { "" }
  | p = TAG_PAD   { p  }

tag_key:
  | id = TAG_IDENT
    { if id = "." then [] else String.split_on_char '.' id }

vtag_inner:
  | t = triple(tag_pad, tag_key, tag_pad) { t }

vtag(TAG_OPEN, TAG_CLOSE):
  | t = delimited(TAG_OPEN, vtag_inner, TAG_CLOSE) { t }

variable:
  | t = vtag(TAG_OPEN_DOUBLE,   TAG_CLOSE_DOUBLE) { Variable t }
  | t = vtag(TAG_OPEN_UNESCAPE, TAG_CLOSE_DOUBLE) { RawVariable t }
  | t = vtag(TAG_OPEN_TRIPLE,   TAG_CLOSE_TRIPLE) { RawVariable t }

ptag_inner:
  | t = triple(tag_pad, TAG_IDENT, tag_pad) { t }

ptag(TAG_OPEN, TAG_CLOSE):
  | t = delimited(TAG_OPEN, ptag_inner, TAG_CLOSE) { t }

partial:
  | t = ptag(TAG_OPEN_PARTIAL,  TAG_CLOSE_DOUBLE) { Partial t }

section_block(X):
  | section_begin = vtag(X, TAG_CLOSE_DOUBLE)
    section_nodes = mustache*
    section_end = vtag(TAG_OPEN_END, TAG_CLOSE_DOUBLE)
    { block(section_begin, (NodeList section_nodes), section_end) }

section:
  | v = section_block(TAG_OPEN_BEGIN) { Section v }
  | v = section_block(TAG_OPEN_BEGIN_INVERTED) { InvertedSection v }

mustache:
  | c = COMMENT { Comment c }
  | x = TEXT { Text x }
  | v = variable { v }
  | p = partial { p }
  | s = section { s }

parse:
  | xs = mustache* EOF { NodeList xs }

%%
