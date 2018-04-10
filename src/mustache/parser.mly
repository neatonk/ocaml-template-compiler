%{
    open Types
    open Types.Mustache
%}

%token EOF
%token <string> TEXT
%token <string> TAG_OPEN
%token <string> TAG_CLOSE
%token <string> TAG_KEY
%token <string> TAG_KEY_SEP

%start <Types.Mustache.t> parse

%%

tag_key:
  | k = separated_nonempty_list(TAG_KEY_SEP, TAG_KEY) { k }

tag:
  | k = delimited(TAG_OPEN, tag_key, TAG_CLOSE) { Variable k }

mustache:
  | x = TEXT { Text x }
  | t = tag { t }

parse:
  | m = list(mustache) EOF { List m }

%%
