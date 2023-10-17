type foo = { x : int; y : string } [@@deriving diff]
type bar = { z : float; foo : foo } [@@deriving diff]
type recursive = { v : int; r : recursive option } [@@deriving diff]

let bar () =
  let bar = { foo = { x = 42; y = "Skolem" }; z = 1. } in
  let f = Diff.Field.Infix.(Bar_foo --| Foo_x) in
  let x' = 10 in
  let () = Format.printf "Setting %a: %d\n%!" Diff.Field.pp f x' in
  let bar' = Diff.Field.set bar f x' in
  let x = Diff.Field.get bar' f in
  Format.printf "Getting %a: %d\n%!" Diff.Field.pp f x

let recursive () =
  let recursive = { v = 1; r = Some { v = 2; r = None } } in
  let f = Diff.Field.Infix.(Recursive_r --| ?*Recursive_r --| ?+Recursive_v) in
  let v' = Some 3 in
  let () =
    Format.printf "Setting %a: %a\n%!" Diff.Field.pp f
      (Format.pp_print_option Format.pp_print_int)
      v'
  in
  let recursive' = Diff.Field.set recursive f v' in
  let v = Diff.Field.get recursive' f in
  Format.printf "Getting %a: %a\n%!" Diff.Field.pp f
    (Format.pp_print_option Format.pp_print_int)
    v

let foo_spec =
  Diff.(
    many
      [
        leaf ~field:Foo_x ~equal:Int.equal;
        leaf ~field:Foo_y ~equal:String.equal;
      ])

let bar_spec =
  Diff.(
    many
      [
        leaf ~field:Bar_z ~equal:Float.equal;
        child ~field:Bar_foo ~spec:foo_spec;
      ])

let bar_diff () =
  let bar0, bar1 =
    ( { z = 2.; foo = { x = 0xdeadbeef; y = "Skolem" } },
      { z = 5.; foo = { x = 0x1337; y = "Skolem" } } )
  in
  let diff = Diff.compute bar0 bar1 bar_spec in
  Format.printf "Computed diff:\n";
  List.iter
    (fun (Diff.Diff { field; _ }) -> Format.printf "\t%a\n" Diff.Field.pp field)
    diff;
  let bar0' = List.fold_left Diff.apply bar0 diff in
  Format.printf "bar0' = bar1? %b\n%!" (bar0' = bar1)

let () =
  bar ();
  recursive ();
  bar_diff ()
