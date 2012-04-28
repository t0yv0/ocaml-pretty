module P = Pretty

let ( <> ) a b = P.append a b

let bracket l x r =
  P.group (
    P.text l
    <> P.indent 2 (P.line " " <> x)
    <> P.line " "
    <> P.text r
  )

type tree =
  | Node of string * tree list

let rec show_tree (Node (s, ts)) =
  let rec show_trees xs =
    match xs with
      | [] -> P.empty
      | [t] -> show_tree t
      | t :: ts -> show_tree t <> P.text "," <> P.line " " <> show_trees ts in
  let show_bracket xs =
    match xs with
      | [] -> P.empty
      | ts -> P.text "[" <> P.indent 1 (show_trees ts) <> P.text "]" in
  P.group (P.text s <> P.indent (String.length s) (show_bracket ts))

let rec show_tree' (Node (s, ts)) =
  let rec show_trees ts =
    match ts with
      | [] -> P.empty
      | [t] -> show_tree' t
      | t :: ts -> show_tree' t <> P.text "," <> P.line " " <> show_trees ts in
  let show_bracket x =
    match x with
      | [] -> P.empty
      | ts -> bracket "[" (show_trees ts) "]" in
  P.text s <> show_bracket ts

let tree =
  let node x y = Node (x, y) in
  node "aaa" [
    node "bbbbb" [
      node "ccc" [];
      node "dd" [];
    ];
    node "eee" [];
    node "ffff" [
      node "gg" [];
      node "hhh" [];
      node "ii" [];
    ];
  ]

let _ =
  P.print ~width:20 (show_tree tree);
  print_string "\n";
  P.print ~width:20 (show_tree' tree);
  print_string "\n";

