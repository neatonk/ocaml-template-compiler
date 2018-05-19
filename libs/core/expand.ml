open Ast

module Ctx = Env.Context (* XXX TODO do not expose *)

(* XXX TODO *)
let escape x = x

let expand_variable (env : Env.t) (t : vtag) =
  let (_, ks, _) = t in
  match Ctx.find_string env.context ks with
  | Some x -> Text (escape x)
  | None   -> Variable t

let expand_raw_variable (env : Env.t) (t : vtag) =
  let (_, ks, _) = t in
  match Ctx.find_string env.context ks with
  | Some x -> Text x
  | None   -> RawVariable t

let expand_partial (env : Env.t) (t : ptag) =
  let (_, n, _) = t in
  match Env.Partials.find_opt n env.partials with
  | None -> Partial t
  | Some tpl -> tpl

(* TODO refactor section code and detect empty values *)
let rec expand_section (env : Env.t) b =
  let (t, xs, _) = b in
  let (_, ks, _) = t in
  match Ctx.find_value env.context ks with
  | None -> Section b
  | Some v ->
    (* XXX TODO include value under empty key so it can used *)
    match v with
    | `Null -> Text ""
    | `Bool b ->
      begin match b with
        | true -> expand env xs
        | false -> Text ""
      end
    | `Float _ -> expand env xs
    | `String _ -> expand env xs
    | `A vl -> NodeList (List.map (fun context -> expand {env with context} xs) vl)
    | `O _ as context -> expand {env with context} xs
and expand_inverted_section (env : Env.t) b =
  let (t, xs, _) = b in
  let (_, ks, _) = t in
  match Ctx.find_value env.context ks with
  | None -> InvertedSection b
  | Some v ->
    match v with
    | `Null -> expand env xs
    | `Bool b ->
      begin match b with
        | true -> Text ""
        | false -> expand env xs
      end
    | `Float _ -> Text ""
    | `String _ -> Text ""
    | `A _ -> Text ""
    | `O _ -> Text ""
and expand env = function
  | NodeList xs       -> NodeList (List.map (expand env) xs)
  | Comment _ as n    -> n
  | Text _ as n       -> n
  | Variable t        -> expand_variable         env t
  | RawVariable t     -> expand_raw_variable     env t
  | Partial t         -> expand_partial          env t
  | Section b         -> expand_section          env b
  | InvertedSection b -> expand_inverted_section env b



let%expect_test "expand (empty)" =
  print_newline ();
  (NodeList
     [(Comment " This is a comment ");
      (Text "This is a line of text\n");
      (Variable ("", ["name"], ""));
      (RawVariable ("", ["name"], ""));
      (Partial ("", "myPartial", ""));
      (Section
         (("", ["items"], ""),
          (NodeList
             [(Text "- ");
              (Variable ("", [], ""));
              (Text "")]),
          ("", ["items"], "")));
      (InvertedSection
         (("", ["items"], ""),
          (NodeList [(Text "(EMPTY)")]),
          ("", ["items"], "")))])
  |> expand Env.empty
  |> Ast.show
  |> print_string;
  [%expect {|
    (Ast.NodeList
       [(Ast.Comment " This is a comment ");
         (Ast.Text "This is a line of text\n");
         (Ast.Variable ("", ["name"], "")); (Ast.RawVariable ("", ["name"], ""));
         (Ast.Partial ("", "myPartial", ""));
         (Ast.Section
            (("", ["items"], ""),
             (Ast.NodeList
                [(Ast.Text "- "); (Ast.Variable ("", [], "")); (Ast.Text "")]),
             ("", ["items"], "")));
         (Ast.InvertedSection
            (("", ["items"], ""), (Ast.NodeList [(Ast.Text "(EMPTY)")]),
             ("", ["items"], "")))
         ]) |}]

let%expect_test "expand (non-empty)" =
  let partials =
    [("myPartial", (Ast.Text "my partial"))]
    |> List.to_seq
    |> Env.Partials.of_seq in
  let context = Env.Context.J.value (
      `O [("name", `String "name")]
    ) in
  let env = Env.({partials; context}) in
  print_newline ();
  (NodeList
     [(Comment " This is a comment ");
      (Text "This is a line of text\n");
      (Variable ("", ["name"], ""));
      (RawVariable ("", ["name"], ""));
      (Partial ("", "myPartial", ""));
      (Section
         (("", ["items"], ""),
          (NodeList
             [(Text "- ");
              (Variable ("", [], ""));
              (Text "")]),
          ("", ["items"], "")));
      (InvertedSection
         (("", ["items"], ""),
          (NodeList [(Text "(EMPTY)")]),
          ("", ["items"], "")))])
  |> expand env
  |> Ast.show
  |> print_string;
  [%expect {|
    (Ast.NodeList
       [(Ast.Comment " This is a comment ");
         (Ast.Text "This is a line of text\n"); (Ast.Text "name");
         (Ast.Text "name"); (Ast.Text "my partial");
         (Ast.Section
            (("", ["items"], ""),
             (Ast.NodeList
                [(Ast.Text "- "); (Ast.Variable ("", [], "")); (Ast.Text "")]),
             ("", ["items"], "")));
         (Ast.InvertedSection
            (("", ["items"], ""), (Ast.NodeList [(Ast.Text "(EMPTY)")]),
             ("", ["items"], "")))
         ]) |}]
