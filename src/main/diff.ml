module Utils = struct
  let pp_const : 'a. string -> Format.formatter -> 'a -> unit =
   fun str fmt _ -> Format.pp_print_string fmt str
end

module Field = struct
  type (_, _) t = ..
  type (_, _) t += Cons : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
  type abstr = Field : (_, _) t -> abstr [@@unboxed]
  type getter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b option } [@@unboxed]
  type setter = { f : 'a 'b. 'a -> ('a, 'b) t -> 'b -> 'a option } [@@unboxed]

  let getter_setter_map = Hashtbl.create 64
  let name_map = Hashtbl.create 64

  let cons : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun l r -> Cons (l, r)

  let add : type a b. (a, b) t -> getter -> setter -> unit =
   fun field getter setter ->
    Hashtbl.replace getter_setter_map (Field field) (getter, setter)

  let set_name : type a b. (a, b) t -> string -> unit =
   fun field str -> Hashtbl.replace name_map (Field field) str

  let rec pp : type a b. Format.formatter -> (a, b) t -> unit =
   fun fmt -> function
    | Cons (l, r) ->
        pp fmt l;
        Format.fprintf fmt " -> ";
        pp fmt r
    | field ->
        let name_opt = Hashtbl.find_opt name_map (Field field) in
        Format.pp_print_option
          ~none:(Utils.pp_const "(unknown)")
          Format.pp_print_string fmt name_opt

  module Infix = struct
    let ( --| ) = cons
  end
end

type error = Missing_field : (_, _) Field.t -> error

exception Diff of error

let rec get_opt : type a b. a -> (a, b) Field.t -> b option =
 fun v field ->
  match field with
  | Field.Cons (l, r) ->
      let v' = get_opt v l in
      Option.bind v' (fun v -> get_opt v r)
  | _ ->
      let g = Hashtbl.find_opt Field.getter_setter_map (Field field) in
      Option.bind g (fun (g, _) -> g.f v field)

let get : type a b. a -> (a, b) Field.t -> b =
 fun v field ->
  match get_opt v field with
  | Some res -> res
  | None -> raise (Diff (Missing_field field))

let rec set_opt : type a b. a -> (a, b) Field.t -> b -> a option =
 fun v field x ->
  match field with
  | Field.Cons (l, r) ->
      let v' = get_opt v l in
      let r = Option.bind v' (fun v -> set_opt v r x) in
      Option.bind r (fun x -> set_opt v l x)
  | _ ->
      let s = Hashtbl.find_opt Field.getter_setter_map (Field field) in
      Option.bind s (fun (_, s) -> s.f v field x)

let set : type a b. a -> (a, b) Field.t -> b -> a =
 fun v field x ->
  match set_opt v field x with
  | Some res -> res
  | None -> raise (Diff (Missing_field field))
