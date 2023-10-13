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

  type exn +=
    | Unknown_field : (_, _) t -> exn
    | Getter_invalid : (_, _) t -> exn
    | Setter_invalid : (_, _) t -> exn

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
          |> Option_ext.value_lazy ~f:(fun () -> raise (Unknown_field field))
        in
        g.f v field
        |> Option_ext.value_lazy ~f:(fun () -> raise (Getter_invalid field))

  let get_opt : type a b. a -> (a, b) t -> b option =
   fun v field -> try Some (get v field) with _ -> None

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
          |> Option_ext.value_lazy ~f:(fun () -> raise (Unknown_field field))
        in
        s.f v field x
        |> Option_ext.value_lazy ~f:(fun () -> raise (Setter_invalid field))

  let set_opt : type a b. a -> (a, b) t -> b -> a option =
   fun v field x -> try Some (set v field x) with _ -> None

  module Infix = struct
    let ( --| ) = cons
  end
end

type _ spec =
  | Leaf : { field : ('a, 'b) Field.t; equal : 'b -> 'b -> bool } -> 'a spec
  | Child : { field : ('a, 'b) Field.t; spec : 'b spec } -> 'a spec
  | Many : 'a spec list -> 'a spec

type _ t = Diff : { field : ('a, 'b) Field.t; new_ : 'b } -> 'a t

let compute : 'a. 'a -> 'a -> 'a spec -> 'a t list =
  let rec helper : type b. b -> b -> b spec -> b t list =
   fun v0 v1 spec ->
    match spec with
    | Leaf { field; equal } ->
        let f0 = Field.get v0 field in
        let f1 = Field.get v1 field in
        if equal f0 f1 then [] else [ Diff { field; new_ = f1 } ]
    | Child { field; spec } ->
        let c0 = Field.get v0 field in
        let c1 = Field.get v1 field in
        let l = helper c0 c1 spec in
        List.map
          (fun (Diff { field = f; new_ }) ->
            Diff { field = Field.cons field f; new_ })
          l
    | Many ls -> List.map (fun l -> helper v0 v1 l) ls |> List.concat
  in
  helper

let apply : type a. a -> a t -> a =
 fun v (Diff { field; new_ }) -> Field.set v field new_
