module T = Tezt
open T.Base
open Test_utils

let () =
  T.Test.register ~__FILE__ ~title:"compute diff with single field"
    ~tags:[ "diff" ]
  @@ fun () ->
  let open struct
    type t = { i : int } [@@deriving diff, show, eq]
  end in
  let typ = Tezt.Check.equalable pp equal in
  let spec = Diff.leaf ~field:I ~equal:Int.equal in
  let t0 = { i = 123 } in
  let t1 = { i = 456 } in
  let diff = Diff.compute t0 t1 spec in
  let fields = List.map field_from_diff diff in
  let expected = [ F I ] in
  T.Check.(
    (fields = expected) ~__LOC__
      (list (field ()))
      ~error_msg:"expected field %R in diff, got %L");
  let t0' = Diff.apply_all t0 diff in
  T.Check.(
    (t0' = t1) ~__LOC__ typ
      ~error_msg:"expected t0 to be %R after diff application");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"compute diff with multiple levels"
    ~tags:[ "diff"; "nested" ]
  @@ fun () ->
  let open struct
    type x = { i : int }
    and y = { x : x; j : float } [@@deriving diff, show, eq]
  end in
  let typ = Tezt.Check.equalable pp_y equal_y in
  let x_spec = Diff.leaf ~field:X_i ~equal:Int.equal in
  let y_spec =
    Diff.(
      many [ leaf ~field:Y_j ~equal:Float.equal; child ~field:Y_x ~spec:x_spec ])
  in
  let y0 = { j = 1.0; x = { i = 1 } } in
  let y1 = { j = 2.0; x = { i = 2 } } in
  let diff = Diff.compute y0 y1 y_spec in
  let fields = List.map field_from_diff diff in
  let expected = Diff.Field.Infix.[ F (Y_x --| X_i); F Y_j ] in
  T.Check.(
    (fields = expected) ~__LOC__
      (list (field ()))
      ~error_msg:"expected field %R in diff, got %L");
  let y0' = Diff.apply_all y0 diff in
  T.Check.(
    (y0' = y1) ~__LOC__ typ
      ~error_msg:"expected t0 to be %R after diff application");
  unit

let () =
  T.Test.register ~__FILE__ ~title:"compute diff with optional child"
    ~tags:[ "diff"; "nested" ]
  @@ fun () ->
  let open struct
    type x = { i : int }
    and y = { x : x option }
    and z = { y : y option } [@@deriving diff, show, eq]
  end in
  let typ = Tezt.Check.equalable pp_z equal_z in
  let x_spec = Diff.leaf ~field:X_i ~equal:Int.equal in
  let y_spec = Diff.opt_child ~field:Y_x ~spec:x_spec in
  let z_spec = Diff.opt_child ~field:Z_y ~spec:y_spec in
  let z0 = { y = Some { x = Some { i = 123 } } } in
  let z1 = { y = Some { x = Some { i = 456 } } } in
  let diff = Diff.compute z0 z1 z_spec in
  let fields = List.map field_from_diff diff in
  let expected = Diff.Field.Infix.[ F (Z_y --| ?+(Y_x --| ?+X_i)) ] in
  T.Check.(
    (fields = expected) ~__LOC__
      (list (field ()))
      ~error_msg:"expected field %R in diff, got %L");
  let z0' = Diff.apply_all z0 diff in
  T.Check.(
    (z0' = z1) ~__LOC__ typ
      ~error_msg:"expected z0 to be %R after diff application");
  unit
