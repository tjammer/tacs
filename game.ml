open Containers
open Moves
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))
module ImMoves = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Movekey))

let ( let* ) = Option.bind

type kind = Pawn | King [@@deriving eq]

module Team = struct
  type t = Blue | Red [@@deriving eq]

  let flip = function Blue -> Red | Red -> Blue

  let to_str = function Blue -> "Blue" | Red -> "Red"

  let win_field = function
    | Blue -> Tile.{ x = 2; y = 0 }
    | Red -> { x = 2; y = 4 }
end

type state =
  | Choose_move
  | Choose_ent of Movekey.t
  | Move of Movekey.t * Tile.Coord.t
  | Over of Team.t

module Transition = struct
  type do_move = { move : Movekey.t; src : Tile.Coord.t; dst : Tile.Coord.t }

  type t = Do_move of do_move | Restart of Team.t | State of state
end

module Input = struct
  type move_select = Left | Right

  type t =
    | Select of [ `Ent of Tile.Coord.t | `Move of move_select ]
    | Deselect
end

let choose_ent tbl team = function
  | `Ent coord ->
      let* _, t = ImCoords.find_opt tbl coord in
      if Team.equal team t then Some coord else None
  | `Move _ -> None

let choose_move team = function
  | `Ent _ -> None
  | `Move Input.Left ->
      Some (match team with Team.Blue -> Movekey.Blue_left | Red -> Red_left)
  | `Move Right -> Some (match team with Blue -> Blue_right | Red -> Red_right)

let outcome_of_selected tbl (move, selected) team dst =
  let* kind, _ = ImCoords.find_opt tbl selected in
  let winning_move =
    Tile.Coord.equal dst (Team.win_field team) && equal_kind kind King
  in
  if Move.is_valid ~src:selected ~dst move then
    match ImCoords.find_opt tbl dst with
    | Some (other_kind, other_team) ->
        if not (Team.equal team other_team) then
          if equal_kind other_kind King || winning_move then Some `Win_take
          else Some `Take
        else None
    | None -> if winning_move then Some `Win_move else Some `Move
  else None

let transitions input tbl state team moves =
  let* input = input in
  let open Transition in
  match (input, state) with
  | Input.Select sel, Choose_move ->
      let* move = choose_move team sel in
      Some (State (Choose_ent move))
  | Select sel, Choose_ent move ->
      let* coord = choose_ent tbl team sel in
      Some (State (Move (move, coord)))
  | Select sel, Move (move, src) -> (
      let* coord = match sel with `Ent c -> Some c | `Move _ -> None in
      let* move_ = ImMoves.find_opt moves move in
      let* outcome = outcome_of_selected tbl (move_, src) team coord in
      match outcome with
      | `Take | `Move -> Some (Do_move { move; src; dst = coord })
      | `Win_take | `Win_move -> Some (State (Over (Team.flip team))) )
  | Select _, Over _ -> None
  | Deselect, Choose_move -> None
  | Deselect, Choose_ent _ -> Some (State Choose_move)
  | Deselect, Move (move, _) -> Some (State (Choose_ent move))
  | Deselect, Over team -> Some (Restart team)

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
    let module Movetbl = Hashtbl.Make (Movekey) in
    let m = Moves.distribute_initial all_moves in

    ( if Team.equal starting_team Team.Red then
      match Movetbl.find_opt m Movekey.Middle with
      | Some move -> Movetbl.replace m Movekey.Middle (Move.flip move)
      | None -> (* TODO log *) () );
    m
  in

  (tbl, moves)

module Mut = struct
  module Movetbl = Hashtbl.Make (Movekey)
  module Coordtbl = Hashtbl.Make (Tile.Coord)

  let advance move moves =
    let middle = ImMoves.find_exn moves Movekey.Middle in
    let value = ImMoves.find_exn moves move in
    Movetbl.replace moves Movekey.Middle (Move.flip value);
    Movetbl.replace moves move middle

  let copy_tbl ~src ~dst =
    (* clears dst *)
    let module Coordtbl = Hashtbl.Make (Tile.Coord) in
    Coordtbl.clear dst;
    Coordtbl.add_seq dst (Coordtbl.to_seq src);
    ()

  let apply transitions tbl state team moves =
    let open Transition in
    match transitions with
    | Some (State s) -> (s, team, moves)
    | Some (Do_move { move; src; dst }) ->
        let ent = Coordtbl.find tbl src in
        let () = advance move moves in
        Coordtbl.replace tbl dst ent;
        Coordtbl.remove tbl src;
        (Choose_move, Team.flip team, moves)
    | Some (Restart team) ->
        let src, moves = restart team in
        copy_tbl ~src ~dst:tbl;
        (Choose_move, team, moves)
    | None -> (state, team, moves)
end
