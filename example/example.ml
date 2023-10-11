type foo = { x : int; y : string } [@@deriving diff]
type bar = { foo : foo } [@@deriving diff]

let my_bar = { foo = { x = 42; y = "Skolem" } }

let () =
  let f = Diff.Infix.(Bar_foo --| Foo_x) in
  let my_bar' = Diff.set my_bar f 10 in
  let x = Diff.get my_bar' f in
  print_endline (string_of_int x)
