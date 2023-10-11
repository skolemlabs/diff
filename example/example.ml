type foo = { x : int; y : string } [@@deriving diff]
type bar = { foo : foo } [@@deriving diff]

let my_bar = { foo = { x = 42; y = "Skolem" } }

let () =
  let f = Diff.Field.Infix.(Bar_foo --| Foo_x) in
  let v = 10 in
  let () = Format.printf "Setting %a: %d\n%!" Diff.Field.pp f v in
  let my_bar' = Diff.set my_bar f v in
  let x = Diff.get my_bar' f in
  Format.printf "Getting %a: %d\n%!" Diff.Field.pp f x
