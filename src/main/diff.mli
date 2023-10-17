module Field : sig
  (** This module contains what might be described as faux lenses. This includes support for composition, getting, setting, etc. *)

  type (_, _) t = ..
  (** An open type that represents a field on a record.

      For example,
      {[
type t = {
  x : int
}
      ]}
      would be described as [X : (t, int) field]. *)

  type getter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b option } [@@unboxed]
  (** A polymorphic function that returns the field's value from the type. *)

  type setter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b -> 'a option } [@@unboxed]
  (** A polymorphic function that returns the type with the field set to the value. *)

  type error =
    | Unknown_field : (_, _) t -> error
    | Getter_invalid : (_, _) t -> error
    | Setter_invalid : (_, _) t -> error

  exception Diff_field of error

  val name : ('a, 'b) t -> string option
  (** [name f] returns the previously registered name for [f], if one exists. *)

  val cons : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** [cons l r] is a field that first indexes [l] and then [r]. *)

  val opt_map : ('a, 'b) t -> ('a option, 'b option) t
  (** [opt_map f] turns [f] into an optional field. *)

  val opt_bind : ('a, 'b option) t -> ('a option, 'b option) t
  (** [opt_map f] monadically binds [f] into an optional field. *)

  val register : ?name:string -> ('a, 'b) t -> getter -> setter -> unit
  (** [register ?name field getter setter] registers [getter] and [setter] (and optionally [name]) to [field]. *)

  val pp : Format.formatter -> ('a, 'b) t -> unit
  val pp_error : Format.formatter -> error -> unit

  module Infix : sig
    val ( --| ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
    (** [f0 --| f1] is [cons f0 f1]. *)

    val ( ?+ ) : ('a, 'b) t -> ('a option, 'b option) t
    (** [?+f] is [opt_map f]. *)

    val ( ?* ) : ('a, 'b option) t -> ('a option, 'b option) t
    (** [?*f] is [opt_bind f]. *)
  end

  val get : 'a -> ('a, 'b) t -> 'b
  (** [get v field] gets [field] from [v], raising [Diff_field error] on exception. *)

  val get_opt : 'a -> ('a, 'b) t -> 'b option
  (** [get_opt v field] gets [field] from [v], returning [None] on exception. *)

  val get_res : 'a -> ('a, 'b) t -> ('b, [> `Diff_field of error ]) result
  (** [get_opt v field] gets [field] from [v], returning [Error (`Diff_field e)] on exception. *)

  val set : 'a -> ('a, 'b) t -> 'b -> 'a
  (** [set v field x] sets [field] in [v] to [x], raising [Diff_field error] on exception *)

  val set_opt : 'a -> ('a, 'b) t -> 'b -> 'a option
  (** [set_opt v field x] sets [field] in [v] to [x], returning [None] on exception *)

  val set_res : 'a -> ('a, 'b) t -> 'b -> ('a, [> `Diff_field of error ]) result
  (** [set_opt v field x] sets [field] in [v] to [x], returning [Error (`Diff_field e)] on exception *)
end

type _ spec
(** Specifies how the record should be traversed when a diff is computed. *)

val leaf : field:('a, 'b) Field.t -> equal:('b -> 'b -> bool) -> 'a spec
(** [leaf ~field ~equal] is a terminal node in the spec tree.

    When computing a diff, the fields are gotten using {!Field.get} and compared with [equal].
    If the values are not equal, the diff includes the new value. *)

val child : field:('a, 'b) Field.t -> spec:'b spec -> 'a spec
(** [child ~field ~spec] is an intermediate node in the spec tree. 

    When computing a diff, the diff for the children is recursively computed. *)

val opt_child : field:('a, 'b option) Field.t -> spec:'b spec -> 'a spec
(** [opt_child ~field ~spec] is an optional intermediate node in the spec tree.

    When computing a diff, the diff for the children is recursively computed
    {i if} the child is present. *)

val many : 'a spec list -> 'a spec
(** [many specs] combines [specs] into a single spec *)

(** Represents a difference between two records of the same type. *)
type _ t = Diff : { field : ('a, 'b) Field.t; new_ : 'b } -> 'a t

val compute : 'a -> 'a -> 'a spec -> 'a t list
(** [compute v0 v1 spec] returns a list of all the differences between [v0] and [v1] using [spec] to traverse the values. *)

val apply : 'a -> 'a t -> 'a
(** [apply v diff] returns [v] with [diff] applied *)

val apply_all : 'a -> 'a t list -> 'a
(** [apply_all v diffs] returns [v] with all [diffs] applied *)
