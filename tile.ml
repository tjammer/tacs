open Containers

type vec2 = { x : int; y : int } [@@deriving eq]

type layout = { size : vec2; origin : vec2 }

module Coord = struct
  type t = vec2 [@@deriving eq]

  let add a b = { x = a.x + b.x; y = a.y + b.y }

  let of_px x y layout =
    {
      x = Int.of_float (x -. Float.of_int layout.origin.x) / layout.size.x;
      y = Int.of_float (y -. Float.of_int layout.origin.y) / layout.size.y;
    }

  (** [to_px coord layout] returns topleft pixel (x, y) of the tile *)
  let to_px coord layout =
    let x = (coord.x * layout.size.x) + layout.origin.x in
    let y = (coord.y * layout.size.y) + layout.origin.y in
    (x, y)

  let hash = Hashtbl.hash
end
