type doc = {

  (** The document node *)
  node : node;

  (** Document size when flattened *)
  flat_size : int;

  (** Minimal width of the first line *)
  min_width : int;

  (** True if document contains no newline nodes *)
  single_line : bool;
}

and node =
  | Append of doc * doc
  | Empty
  | Group of doc
  | Indent of int * doc
  | Line of int * string (* int = String.length string *)
  | Text of int * string (* int = String.length string *)

let append a b =
  match a.node, b.node with
    | Empty, _ -> b
    | _, Empty -> a
    | _ ->
      {
        node = Append (a, b);
        flat_size = a.flat_size + b.flat_size;
        min_width =
          if a.single_line
          then a.min_width + b.min_width
          else a.min_width;
        single_line = a.single_line && b.single_line;
      }

let empty =
  {
    node = Empty;
    flat_size = 0;
    min_width = 0;
    single_line = true;
  }

let group x =
  { x with node = Group x }

let indent k x =
  { x with node = Indent (k, x) }

let line x =
  let n = String.length x in
  {
    node = Line (n, x);
    flat_size = n;
    min_width = 0;
    single_line = false;
  }

let text x =
  let n = String.length x in
  {
    node = Text (n, x);
    flat_size = n;
    min_width = n;
    single_line = true;
  }

let print_indentation n =
  print_char '\n';
  for i = 1 to n do
  print_char ' ';
  done

let rec flatten x =
  match x.node with
    | Append (a, b) -> append (flatten a) (flatten b)
    | Empty | Text _ -> x
    | Group x | Indent (_, x) -> flatten x
    | Line (_, x) -> text x

type stack_node =
    {
      doc: doc;
      min_total: int;
      offset: int;
    }

type stack =
  | N
  | C of stack_node * stack

let min_total stack =
  match stack with
    | N -> 0
    | C (x, _) -> x.min_total

let push o n (stack: stack) =
  let m =
    if n.single_line
    then min_total stack + n.min_width
    else n.min_width in
  C ({ doc = n; offset = o; min_total = m }, stack)

let print
    ?width:(width=70)
    ?output:(output=print_string)
    ?indent:(indent=print_indentation)
    doc =
  let rec pr k x =
    match x with
      | N -> ()
      | C (x, z) ->
        let i = x.offset in
        match x.doc.node with
          | Append (a, b)  ->
            pr k (push i a (push i b z))
          | Empty ->
            pr k z
          | Group x ->
            let y =
              if x.flat_size + min_total z <= width - k
              then flatten x
              else x in
            pr k (push i y z)
          | Indent (j, x) ->
            pr k (push (i + j) x z)
          | Line _ ->
            indent i;
            pr i z
          | Text (len, s) ->
            output s;
            pr (k + len) z in
  pr 0 (push 0 doc N)
