module Mustache = struct
  type t =
    | Text of string
    | Variable of string list
    | List of t list
end
