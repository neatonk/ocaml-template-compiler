module StringMap = Map.Make(String) (* XXX TODO do not expose *)
module type Partials_type = sig
  type t = Ast.t StringMap.t
  type 'a x = 'a StringMap.t
  include (module type of StringMap) with type 'a t := 'a x
end

module Partials : Partials_type = struct
  type 'a x = 'a StringMap.t
  include StringMap
end

(* XXX TODO implement parent context and lookup *)
module Context = struct
  type t = Ezjsonm.value

  let empty = Ezjsonm.value (`O [])

  module J = Ezjsonm (* XXX do not expose *)

  let find ~f data ks =
    try
      Some (J.find data ks |> f)
    with Not_found ->
      None

  let find_value data ks =
    let f = (fun x -> x) in
    find ~f data ks

  let find_string data ks =
    let f = J.get_string in
    find ~f data ks
end

type t = {
  partials: Partials.t;
  context: Context.t
}

let empty = {
  partials = Partials.empty;
  context = Context.empty;
}
