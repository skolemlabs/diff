module Field : sig
  type (_, _) t = ..
  (** An open type that represents a field on a type.

      For example,
      {[
type t = {
  x : int
}
      ]}
      would be described as [X : (t, int) field] *)

  type getter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b option } [@@unboxed]
  (** A polymorphic function that returns the field's value from the type. *)

  type setter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b -> 'a option } [@@unboxed]
  (** A polymorphic function that returns the type with the field set to the value. *)

  type exn +=
    | Unknown_field : (_, _) t -> exn
    | Getter_invalid : (_, _) t -> exn
    | Setter_invalid : (_, _) t -> exn

  val cons : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** [cons l r] is a field that first indexes [l] and then [r]. *)

  val opt_map : ('a, 'b) t -> ('a option, 'b option) t
  (** [opt_map f] turns [f] into an optional field. *)

  val opt_bind : ('a, 'b option) t -> ('a option, 'b option) t
  (** [opt_map f] monadically binds [f] into an optional field. *)

  val register : ?name:string -> ('a, 'b) t -> getter -> setter -> unit
  (** [register ?name field getter setter] registers [getter] and [setter] (and optionally [name]) to [field]. *)

  val pp : Format.formatter -> ('a, 'b) t -> unit

  module Infix : sig
    val ( --| ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  end

  val get : 'a -> ('a, 'b) t -> 'b
  (** [get v field] gets [field] from [v], throwing an error on exception. *)

  val get_opt : 'a -> ('a, 'b) t -> 'b option
  (** [get_opt v field] gets [field] from [v], returning [None] on exception. *)

  val set : 'a -> ('a, 'b) t -> 'b -> 'a
  (** [set v field x] sets [field] in [v] to [x], throwing an error on exception *)

  val set_opt : 'a -> ('a, 'b) t -> 'b -> 'a option
  (** [set_opt v field x] sets [field] in [v] to [x], returning [None] on exception *)
end

type _ spec =
  | Leaf : { field : ('a, 'b) Field.t; equal : 'b -> 'b -> bool } -> 'a spec
  | Child : { field : ('a, 'b) Field.t; spec : 'b spec } -> 'a spec
  | Many : 'a spec list -> 'a spec

type _ t = Diff : { field : ('a, 'b) Field.t; new_ : 'b } -> 'a t

val compute : 'a -> 'a -> 'a spec -> 'a t list
val apply : 'a -> 'a t -> 'a
val apply_all : 'a -> 'a t list -> 'a
