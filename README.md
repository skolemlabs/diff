# diff
An OCaml library for generating diffs of product types.

## Example
```ocaml
type t = { foo : int; bar : string; }
[@@deriving diff]

let spec = Diff.many [
  Diff.leaf ~field:Foo ~equal:Int.equal;
  Diff.leaf ~field:Bar ~equal:String.equal;
]

let () =
  let t0 = { foo = 1; bar = "Skolem" } in
  let t1 = { foo = 1; bar = "OCaml" } in
  let diff = Diff.compute t0 t1 spec in
  let t0' = Diff.apply_all t0 diff in
  assert (t0' = t1)
```

## Usage

The main use case of this library is to version records without having to keep a copy of each version around.

The library includes a ppx for generating field accessors from records. To use the ppx with dune, add it to the `preprocessing` field of your stanza:
```dune
(library
 (name mylib)
 ...
 (preprocessing (pps diff.ppx)))
```

In order to actually compute diffs, you need to write a spec for how your record should be traversed. There are functions provided to help you do so:

- `leaf ~field ~equal` represents a "terminal" node in the spec tree, i.e. the diffing algorithm should simply compare the field values and add it to the diff list if they are different.
- `child ~field ~spec` represents an "intermediate" node in the spec tree, i.e. the diffing algorithm should recursively compute the diff of a "child" record.
- `opt_child ~field ~spec` reprents an *optional* intermediate node in the spec tree, i.e. a child record that is optional in the parent.
- `many specs`: flattens many specs into a single one.

For more usage examples, check out `example/example.ml` and `tests/*_test.ml`.

## License
This project is licensed under the MIT License.
