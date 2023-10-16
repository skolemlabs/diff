module T = Tezt
open T.Base

let () =
  T.Test.register ~__FILE__ ~title:"get field" ~tags:[ "field"; "get" ]
  @@ fun () ->
  let open struct
    type t = { i : int } [@@deriving diff]
  end in
  let t = { i = 123 } in
  let v = Diff.Field.get t I in
  T.Check.((v = 123) ~__LOC__ int ~error_msg:"expected t.i = %R, got %L");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"set field" ~tags:[ "field"; "set" ]
  @@ fun () ->
  let open struct
    type t = { i : int } [@@deriving diff]
  end in
  let t = { i = 0 } in
  let t' = Diff.Field.set t I 123 in
  T.Check.((t'.i = 123) ~__LOC__ int ~error_msg:"expected t.i = %R, got %L");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"get nested field"
    ~tags:[ "field"; "get"; "nested" ]
  @@ fun () ->
  let open struct
    type x = { i : int }
    and y = { x : x } [@@deriving diff]
  end in
  let f = Diff.Field.(Infix.(Y_x --| X_i)) in
  let y = { x = { i = 123 } } in
  let v = Diff.Field.get y f in
  T.Check.((v = 123) ~__LOC__ int ~error_msg:"expected y.x.i = %R, got %L");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"set nested field"
    ~tags:[ "field"; "set"; "nested" ]
  @@ fun () ->
  let open struct
    type x = { i : int }
    and y = { x : x } [@@deriving diff]
  end in
  let f = Diff.Field.(Infix.(Y_x --| X_i)) in
  let y = { x = { i = 0 } } in
  let y' = Diff.Field.set y f 123 in
  T.Check.((y'.x.i = 123) ~__LOC__ int ~error_msg:"expected y.x.i = %R, got %L");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"get field from optional child"
    ~tags:[ "field"; "get"; "nested"; "optional" ]
  @@ fun () ->
  let open struct
    type x = { i : int }
    and y = { x : x option } [@@deriving diff]
  end in
  let f = Diff.Field.(Infix.(Y_x --| opt_map X_i)) in
  let y = { x = Some { i = 123 } } in
  let v = Diff.Field.get y f in
  T.Check.(
    (v = Some 123) ~__LOC__ (option int)
      ~error_msg:"expected y.x.i = %R, got %L");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"get optional field from optional child"
    ~tags:[ "field"; "get"; "nested"; "optional" ]
  @@ fun () ->
  let open struct
    type x = { i : int option }
    and y = { x : x option } [@@deriving diff]
  end in
  let f = Diff.Field.(Infix.(Y_x --| opt_bind X_i)) in
  let y = { x = Some { i = Some 123 } } in
  let v = Diff.Field.get y f in
  T.Check.(
    (v = Some 123) ~__LOC__ (option int)
      ~error_msg:"expected y.x.i = %R, got %L");
  unit
