module Option_ext = struct
  let value_lazy ~f o = match o with Some v -> v | None -> f ()

  module Infix = struct
    let ( >|= ) v f = Option.map f v
    let ( >>= ) v f = Option.bind v f
  end
end

module Utils = struct
  let pp_const : 'a. string -> Format.formatter -> 'a -> unit =
   fun str fmt _ -> Format.pp_print_string fmt str
end

module Field = struct
  type (_, _) t = ..

  type (_, _) t +=
    | Cons : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | Opt_map : ('a, 'b) t -> ('a option, 'b option) t
    | Opt_bind : ('a, 'b option) t -> ('a option, 'b option) t

  type abstr = Field : (_, _) t -> abstr [@@unboxed]
  type getter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b option } [@@unboxed]
  type setter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b -> 'a option } [@@unboxed]

  let getter_setter_map = Hashtbl.create 64
  let name_map = Hashtbl.create 64

  let cons : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun l r -> Cons (l, r)

  let opt_map : type a b. (a, b) t -> (a option, b option) t =
   fun f -> Opt_map f

  let opt_bind : type a b. (a, b option) t -> (a option, b option) t =
   fun f -> Opt_bind f

  let register : type a b. ?name:string -> (a, b) t -> getter -> setter -> unit
      =
   fun ?name field getter setter ->
    Hashtbl.replace getter_setter_map (Field field) (getter, setter);
    Option.iter (Hashtbl.replace name_map (Field field)) name

  let rec pp : type a b. Format.formatter -> (a, b) t -> unit =
   fun fmt t ->
    match t with
    | Cons (l, r) ->
        pp fmt l;
        Format.pp_print_string fmt " -> ";
        pp fmt r
    | Opt_map f ->
        Format.pp_print_char fmt '?';
        pp fmt f
    | Opt_bind f -> pp fmt f
    | field ->
        let name_opt = Hashtbl.find_opt name_map (Field field) in
        Format.pp_print_option
          ~none:(Utils.pp_const "(unknown)")
          Format.pp_print_string fmt name_opt

  module Infix = struct
    let ( --| ) = cons
  end
end

type error =
  | Unknown_field : (_, _) Field.t -> error
  | Getter_invalid : (_, _) Field.t -> error
  | Setter_invalid : (_, _) Field.t -> error

exception Diff of error

let rec get : type a b. a -> (a, b) Field.t -> b =
 fun v field ->
  let open Option_ext.Infix in
  match field with
  | Field.Cons (l, r) ->
      let lv = get v l in
      get lv r
  | Field.Opt_map f -> v >|= fun v -> get v f
  | Field.Opt_bind f -> v >>= fun v -> get v f
  | _ ->
      let g, _ =
        Hashtbl.find_opt Field.getter_setter_map (Field field)
        |> Option_ext.value_lazy ~f:(fun () ->
               raise (Diff (Unknown_field field)))
      in
      g.f v field
      |> Option_ext.value_lazy ~f:(fun () ->
             raise (Diff (Getter_invalid field)))

let get_opt : type a b. a -> (a, b) Field.t -> b option =
 fun v field -> try Some (get v field) with _ -> None

let rec set : type a b. a -> (a, b) Field.t -> b -> a =
 fun v field x ->
  let open Option_ext.Infix in
  match field with
  | Field.Cons (l, r) ->
      let lv = get v l in
      let lv = set lv r x in
      set v l lv
  | Field.Opt_map f ->
      v >>= fun v ->
      x >|= fun x -> set v f x
  | Field.Opt_bind f -> v >|= fun v -> set v f x
  | _ ->
      let _, s =
        Hashtbl.find_opt Field.getter_setter_map (Field field)
        |> Option_ext.value_lazy ~f:(fun () ->
               raise (Diff (Unknown_field field)))
      in
      s.f v field x
      |> Option_ext.value_lazy ~f:(fun () ->
             raise (Diff (Setter_invalid field)))

let set_opt : type a b. a -> (a, b) Field.t -> b -> a option =
 fun v field x -> try Some (set v field x) with _ -> None
