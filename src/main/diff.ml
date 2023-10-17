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

  type error =
    | Unknown_field : (_, _) t -> error
    | Getter_invalid : (_, _) t -> error
    | Setter_invalid : (_, _) t -> error

  exception Diff_field of error

  let getter_setter_map = Hashtbl.create 64
  let name_map = Hashtbl.create 64

  let name : type a b. (a, b) t -> string option =
   fun f -> Hashtbl.find_opt name_map (Field f)

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

  let pp_error fmt = function
    | Unknown_field _ -> Format.pp_print_string fmt "unknown field"
    | Getter_invalid f -> Format.fprintf fmt "getter invalid for field %a" pp f
    | Setter_invalid f -> Format.fprintf fmt "setter invalid for field %a" pp f

  let rec get : type a b. a -> (a, b) t -> b =
   fun v field ->
    let open Option_ext.Infix in
    match field with
    | Cons (l, r) ->
        let lv = get v l in
        get lv r
    | Opt_map f -> v >|= fun v -> get v f
    | Opt_bind f -> v >>= fun v -> get v f
    | _ ->
        let g, _ =
          Hashtbl.find_opt getter_setter_map (Field field)
          |> Option_ext.value_lazy ~f:(fun () ->
                 raise (Diff_field (Unknown_field field)))
        in
        g.f v field
        |> Option_ext.value_lazy ~f:(fun () ->
               raise (Diff_field (Getter_invalid field)))

  let get_opt : type a b. a -> (a, b) t -> b option =
   fun v field -> try Some (get v field) with Diff_field _ -> None

  let get_res : type a b. a -> (a, b) t -> (b, [> `Diff_field of error ]) result
      =
   fun v field ->
    try Ok (get v field) with Diff_field e -> Error (`Diff_field e)

  let rec set : type a b. a -> (a, b) t -> b -> a =
   fun v field x ->
    let open Option_ext.Infix in
    match field with
    | Cons (l, r) ->
        let lv = get v l in
        let lv = set lv r x in
        set v l lv
    | Opt_map f ->
        v >>= fun v ->
        x >|= fun x -> set v f x
    | Opt_bind f -> v >|= fun v -> set v f x
    | _ ->
        let _, s =
          Hashtbl.find_opt getter_setter_map (Field field)
          |> Option_ext.value_lazy ~f:(fun () ->
                 raise (Diff_field (Unknown_field field)))
        in
        s.f v field x
        |> Option_ext.value_lazy ~f:(fun () ->
               raise (Diff_field (Setter_invalid field)))

  let set_opt : type a b. a -> (a, b) t -> b -> a option =
   fun v field x -> try Some (set v field x) with Diff_field _ -> None

  let set_res :
      type a b. a -> (a, b) t -> b -> (a, [> `Diff_field of error ]) result =
   fun v field x ->
    try Ok (set v field x) with Diff_field e -> Error (`Diff_field e)

  module Infix = struct
    let ( --| ) = cons
    let ( ?+ ) = opt_map
    let ( ?* ) = opt_bind
  end
end

type _ spec =
  | Leaf : { field : ('a, 'b) Field.t; equal : 'b -> 'b -> bool } -> 'a spec
  | Child : { field : ('a, 'b) Field.t; spec : 'b spec } -> 'a spec
  | Opt_child : { field : ('a, 'b option) Field.t; spec : 'b spec } -> 'a spec
  | Many : 'a spec list -> 'a spec

let leaf : type a b. field:(a, b) Field.t -> equal:(b -> b -> bool) -> a spec =
 fun ~field ~equal -> Leaf { field; equal }

let child : type a b. field:(a, b) Field.t -> spec:b spec -> a spec =
 fun ~field ~spec -> Child { field; spec }

let opt_child : type a b. field:(a, b option) Field.t -> spec:b spec -> a spec =
 fun ~field ~spec -> Opt_child { field; spec }

let many : type a. a spec list -> a spec = fun v -> Many v

type _ t = Diff : { field : ('a, 'b) Field.t; new_ : 'b } -> 'a t

let compute : 'a. 'a -> 'a -> 'a spec -> 'a t list =
  let rec helper :
      type b c. b -> b -> b spec -> (b t -> c t) -> c t list -> c t list =
   fun v0 v1 spec f acc ->
    match spec with
    | Leaf { field; equal } ->
        let f0 = Field.get v0 field in
        let f1 = Field.get v1 field in
        if equal f0 f1 then acc else f (Diff { field; new_ = f1 }) :: acc
    | Child { field; spec } ->
        let c0 = Field.get v0 field in
        let c1 = Field.get v1 field in
        let f (Diff { field = field'; new_ }) =
          f (Diff { field = Field.cons field field'; new_ })
        in
        (helper [@tailcall]) c0 c1 spec f acc
    | Opt_child { field; spec } -> (
        let o0 = Field.get v0 field in
        let o1 = Field.get v1 field in
        match (o0, o1, field) with
        | None, None, _ -> acc
        | Some _, None, _ | None, Some _, _ ->
            f (Diff { field; new_ = o1 }) :: acc
        | Some c0, Some c1, _ ->
            let f (Diff { field = field'; new_ }) =
              f
                (Diff
                   {
                     field = Field.Cons (field, Field.Opt_map field');
                     new_ = Some new_;
                   })
            in
            (helper [@tailcall]) c0 c1 spec f acc)
    | Many ls -> List.fold_left (fun acc l -> helper v0 v1 l f acc) acc ls
  in
  fun v0 v1 spec -> helper v0 v1 spec Fun.id []

let apply : type a. a -> a t -> a =
 fun v (Diff { field; new_ }) -> Field.set v field new_

let apply_all : type a. a -> a t list -> a =
 fun v ds -> List.fold_left apply v ds
