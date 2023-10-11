type (_, _) field = ..
type (_, _) field += Cons : ('a, 'b) field * ('b, 'c) field -> ('a, 'c) field
type error = Missing_field : (_, _) field -> error

exception Diff of error

type abstr = Field : (_, _) field -> abstr [@@unboxed]
type getter = { f : 'a 'b. 'a -> ('a, 'b) field -> 'b option } [@@unboxed]
type setter = { f : 'a 'b. 'a -> ('a, 'b) field -> 'b -> 'a option } [@@unboxed]

let map = Hashtbl.create 64

let cons : type a b c. (a, b) field -> (b, c) field -> (a, c) field =
 fun l r -> Cons (l, r)

let add : type a b. (a, b) field -> getter -> setter -> unit =
 fun field getter setter ->
  match field with
  | Cons _ -> raise (Invalid_argument "Cons cannot be bound")
  | _ -> Hashtbl.replace map (Field field) (getter, setter)

let rec get_opt : type a b. a -> (a, b) field -> b option =
 fun v field ->
  match field with
  | Cons (l, r) ->
      let v' = get_opt v l in
      Option.bind v' (fun v -> get_opt v r)
  | _ ->
      let g = Hashtbl.find_opt map (Field field) in
      Option.bind g (fun (g, _) -> g.f v field)

let get : type a b. a -> (a, b) field -> b =
 fun v field ->
  match get_opt v field with
  | Some res -> res
  | None -> raise (Diff (Missing_field field))

let rec set_opt : type a b. a -> (a, b) field -> b -> a option =
 fun v field x ->
  match field with
  | Cons (l, r) ->
      let v' = get_opt v l in
      let r = Option.bind v' (fun v -> set_opt v r x) in
      Option.bind r (fun x -> set_opt v l x)
  | _ ->
      let s = Hashtbl.find_opt map (Field field) in
      Option.bind s (fun (_, s) -> s.f v field x)

let set : type a b. a -> (a, b) field -> b -> a =
 fun v field x ->
  match set_opt v field x with
  | Some res -> res
  | None -> raise (Diff (Missing_field field))

module Infix = struct
  let ( --| ) = cons
end
