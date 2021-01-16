open Containers

let inside_field coord =
  Tile.(coord.x) >= 0 && coord.x < 5 && coord.y >= 0 && coord.y < 5

module Move = struct
  type t = Tile.Coord.t list [@@deriving eq]

  let is_valid ~src ~dst move =
    List.fold_while
      (fun accum move ->
        match inside_field dst with
        | true ->
            if
              Tile.Coord.equal
                Tile.{ x = src.x + move.x; y = src.y + move.y }
                dst
            then (true, `Stop)
            else (accum, `Continue)
        | false -> (false, `Stop))
      false move

  let flip = List.map (fun Tile.{ x; y } -> Tile.{ x = -x; y = -y })
end

module Moves = struct
  type key = Blue | Red | Middle [@@deriving eq]

  type t = (key * Move.t list) list

  let distribute_initial moves =
    Random.self_init ();
    let moves =
      Random.get_state ()
      |> Random.sample_without_duplicates ~cmp:compare 5
           (Random.int (List.length moves))
      |> List.map (fun i -> List.nth moves i)
    in

    let blue, moves = List.take_drop 2 moves in
    let red, moves = List.take_drop 2 moves in
    let middle = List.take 1 moves in
    let red = List.map (fun m -> Move.flip m) red in
    [ (Blue, blue); (Red, red); (Middle, middle) ]

  let get_all moves key = List.Assoc.get_exn ~eq:equal_key key moves

  let get_middle moves = get_all moves Middle |> List.hd
end

let mantis = [ { Tile.x = 0; y = 1 }; { x = -1; y = -1 }; { x = 1; y = -1 } ]

let horse = [ { Tile.x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } ]

let tiger = [ { Tile.x = 0; y = 1 }; { x = 0; y = -2 } ]

let ox = [ { Tile.x = 0; y = -1 }; { x = 1; y = 0 }; { x = 0; y = 1 } ]

let cobra = [ { Tile.x = 1; y = 1 }; { x = 1; y = -1 }; { x = -1; y = 0 } ]

let crane = [ { Tile.x = 0; y = -1 }; { x = -1; y = 1 }; { x = 1; y = 1 } ]

let boar = [ { Tile.x = 0; y = -1 }; { x = -1; y = 0 }; { x = 1; y = 0 } ]

let eel = [ { Tile.x = -1; y = -1 }; { x = -1; y = 1 }; { x = 1; y = 0 } ]

let frog = [ { Tile.x = -1; y = -1 }; { x = -2; y = 0 }; { x = 1; y = 1 } ]

let crab = [ { Tile.x = -2; y = 0 }; { x = 0; y = -1 }; { x = 2; y = 0 } ]

let rabbit = [ { Tile.x = -1; y = 1 }; { x = 2; y = 0 }; { x = 1; y = -1 } ]

let monkey =
  [
    { Tile.x = -1; y = -1 };
    { x = -1; y = 1 };
    { x = 1; y = 1 };
    { x = 1; y = -1 };
  ]

let goose =
  [
    { Tile.x = -1; y = 0 };
    { x = -1; y = -1 };
    { x = 1; y = 0 };
    { x = 1; y = 1 };
  ]

let elephant =
  [
    { Tile.x = -1; y = 0 };
    { x = -1; y = -1 };
    { x = 1; y = 0 };
    { x = 1; y = -1 };
  ]

let dragon =
  [
    { Tile.x = -1; y = 1 };
    { x = -2; y = -1 };
    { x = 1; y = 1 };
    { x = 2; y = -1 };
  ]

let all_moves =
  [
    mantis;
    horse;
    tiger;
    ox;
    cobra;
    monkey;
    crane;
    boar;
    frog;
    crab;
    rabbit;
    goose;
    elephant;
    dragon;
  ]
