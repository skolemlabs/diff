type 'a field = F : ('a, _) Diff.Field.t -> 'a field

let field () = Tezt.Check.equalable (fun fmt (F f) -> Diff.Field.pp fmt f) ( = )
let field_from_diff (Diff.Diff { field; _ }) = F field

let field_error : [ `Diff_field of Diff.Field.error ] Tezt.Check.typ =
  Tezt.Check.equalable
    (fun fmt (`Diff_field e) -> Diff.Field.pp_error fmt e)
    ( = )

let result ok error =
  Tezt.Check.(
    convert
      (function Ok v -> (Some v, None) | Error e -> (None, Some e))
      (tuple2 (option ok) (option error)))
