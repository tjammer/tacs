open Containers
open Moves
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))

let ( let* ) = Option.bind

type kind = Pawn | King [@@deriving eq, show]

module Team = struct
  type t = Blue | Red [@@deriving eq]

  let flip = function Blue -> Red | Red -> Blue

  let to_str = function Blue -> "Blue" | Red -> "Red"

  let win_field = function
    | Blue -> Tile.{ x = 2; y = 0 }
    | Red -> { x = 2; y = 4 }

  let to_moves_key = function Blue -> Moves.(Blue) | Red -> Red
end

type state =
  | Choose_move
  | Choose_ent of Move.t
  | Move of Move.t * Tile.Coord.t
  | Over of Team.t

type do_move = { move : Move.t; src : Tile.Coord.t; dst : Tile.Coord.t }

type transition = Do_move of do_move | Restart of Team.t | State of state

type input = Select of Tile.Coord.t | Deselect

let choose_ent tbl coord team =
  (* maybe this can stay here as a safety net *)
  match ImCoords.find_opt tbl coord with
  | Some (kind, t) -> if Team.equal team t then Some kind else None
  | None -> None

let outcome_of_selected tbl (move, selected) team dst =
  let* kind, _ = ImCoords.find_opt tbl selected in
  if Move.is_valid ~src:selected ~dst move then
    match ImCoords.find_opt tbl dst with
    | Some (other_kind, other_team) ->
        if not (Team.equal team other_team) then
          if equal_kind other_kind King then Some `Win_take else Some `Take
        else None
    | None ->
        if Tile.Coord.equal dst (Team.win_field team) && equal_kind kind King
        then Some `Win_move
        else Some `Move
  else None

let advance move team moves =
  let team = Team.to_moves_key team in
  let eq = Moves.equal_key in
  let middle = Moves.get_all moves Middle |> List.hd in
  let moves =
    List.Assoc.update ~eq
      ~f:(function
        | Some [ a; b ] ->
            Some (if Move.equal a move then [ middle; b ] else [ a; middle ])
        | Some a -> Some a
        | None -> None)
      team moves
  in
  List.Assoc.set ~eq Middle [ Move.flip move ] moves

let copy_tbl ~src ~dst =
  (* clears dst *)
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  Coordtbl.clear dst;
  Coordtbl.add_seq dst (Coordtbl.to_seq src);
  ()

let restart starting_team =
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  let tbl = Coordtbl.create 10 in
  Coordtbl.add tbl { Tile.x = 0; y = 4 } (Pawn, Team.Blue);
  Coordtbl.add tbl { Tile.x = 1; y = 4 } (Pawn, Blue);
  Coordtbl.add tbl { Tile.x = 2; y = 4 } (King, Blue);
  Coordtbl.add tbl { Tile.x = 3; y = 4 } (Pawn, Blue);
  Coordtbl.add tbl { Tile.x = 4; y = 4 } (Pawn, Blue);

  Coordtbl.add tbl { Tile.x = 0; y = 0 } (Pawn, Red);
  Coordtbl.add tbl { Tile.x = 1; y = 0 } (Pawn, Red);
  Coordtbl.add tbl { Tile.x = 2; y = 0 } (King, Red);
  Coordtbl.add tbl { Tile.x = 3; y = 0 } (Pawn, Red);
  Coordtbl.add tbl { Tile.x = 4; y = 0 } (Pawn, Red);

  let moves =
    Moves.distribute_initial all_moves
    |> fun m ->
    if Team.equal starting_team Team.Red then
      List.map
        (fun (key, move) ->
          match key with
          | Moves.Middle -> (key, List.map Move.flip move)
          | Red | Blue -> (key, move))
        m
    else m
  in

  (tbl, moves)
