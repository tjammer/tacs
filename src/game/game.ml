open ContainersLabels
module Tile = Tile
module Moves = Moves
open Moves
open Option.Infix

type kind = Pawn | King [@@deriving eq]

module Team = struct
  type t = Blue | Red [@@deriving eq, sexp]

  let flip = function Blue -> Red | Red -> Blue
  let to_str = function Blue -> "Blue" | Red -> "Red"

  let win_field = function
    | Blue -> Tile.{ x = 2; y = 0 }
    | Red -> { x = 2; y = 4 }
end

type state_kind =
  | Choose_move
  | Choose_ent of Movekey.t
  | Move of Movekey.t * Tile.Coord.t
  | Over of Team.t

module State = struct
  type t = {
    ents : (Tile.Coord.t, kind * Team.t) List.Assoc.t;
    moves : (Movekey.t, Move.t) List.Assoc.t;
    state : state_kind;
    curr_team : Team.t;
  }
end

module Transition = struct
  type do_move = { move : Movekey.t; src : Tile.Coord.t; dst : Tile.Coord.t }
  type t = Do_move of do_move | Restart of Team.t | State of state_kind
end

module Input = struct
  type move_select = Left | Right [@@deriving sexp]

  type t =
    | Select of [ `Ent of Tile.Coord.t | `Move of move_select ]
    | Deselect
  [@@deriving sexp]
end

let choose_ent tbl team = function
  | `Ent coord ->
      let* _, t = List.Assoc.get ~eq:Tile.Coord.equal coord tbl in
      if Team.equal team t then Some coord else None
  | `Move _ -> None

let choose_move team = function
  | `Ent _ -> None
  | `Move Input.Left ->
      Some (match team with Team.Blue -> Movekey.Blue_left | Red -> Red_left)
  | `Move Right ->
      Some (match team with Blue -> Blue_right | Red -> Red_right)

type outcome = Take | Move | Win_take | Win_move

let outcome_of_selected gs (move, selected) dst =
  let* kind, _ = List.Assoc.get ~eq:Tile.Coord.equal selected State.(gs.ents) in
  let winning_move =
    Tile.Coord.equal dst (Team.win_field gs.curr_team) && equal_kind kind King
  in
  if Move.is_valid ~src:selected ~dst move then
    match List.Assoc.get ~eq:Tile.Coord.equal dst gs.ents with
    | Some (other_kind, other_team) ->
        if not (Team.equal gs.curr_team other_team) then
          if equal_kind other_kind King || winning_move then Some Win_take
          else Some Take
        else None
    | None -> if winning_move then Some Win_move else Some Move
  else None

let transitions input gs =
  let* input = input in
  let open Transition in
  match (input, State.(gs.state)) with
  | Input.Select sel, Choose_move ->
      let* move = choose_move gs.curr_team sel in
      Some (State (Choose_ent move))
  | Select sel, Choose_ent move ->
      let* coord = choose_ent gs.ents gs.curr_team sel in
      Some (State (Move (move, coord)))
  | Select sel, Move (move, src) -> (
      let* coord = match sel with `Ent c -> Some c | `Move _ -> None in
      let* move_ = List.Assoc.get ~eq:Movekey.equal move gs.moves in
      outcome_of_selected gs (move_, src) coord >>= function
      | Take | Move -> Some (Do_move { move; src; dst = coord })
      | Win_take | Win_move ->
          let team = Team.flip gs.curr_team in
          Some (State (Over team)))
  | Select _, Over _ -> None
  | Deselect, Choose_move -> None
  | Deselect, Choose_ent _ -> Some (State Choose_move)
  | Deselect, Move (move, _) -> Some (State (Choose_ent move))
  | Deselect, Over team -> Some (Restart team)

let init starting_team seed =
  let ents =
    [
      ({ Tile.x = 0; y = 4 }, (Pawn, Team.Blue));
      ({ x = 1; y = 4 }, (Pawn, Blue));
      ({ x = 2; y = 4 }, (King, Blue));
      ({ x = 3; y = 4 }, (Pawn, Blue));
      ({ x = 4; y = 4 }, (Pawn, Blue));
      ({ x = 0; y = 0 }, (Pawn, Red));
      ({ x = 1; y = 0 }, (Pawn, Red));
      ({ x = 2; y = 0 }, (King, Red));
      ({ x = 3; y = 0 }, (Pawn, Red));
      ({ x = 4; y = 0 }, (Pawn, Red));
    ]
  in

  let moves =
    let module Movetbl = Hashtbl.Make (Movekey) in
    let m = Moves.distribute_initial all_moves seed in

    if Team.equal starting_team Team.Red then
      List.Assoc.update ~eq:Movekey.equal Movekey.Middle m ~f:(function
        | Some move -> Some (Move.flip move)
        | None -> failwith "cannot flip middle")
    else m
  in

  State.{ ents; moves; state = Choose_move; curr_team = starting_team }

module Mut = struct
  let advance move moves =
    let eq = Movekey.equal in
    let middle = List.Assoc.get_exn ~eq Movekey.Middle moves in
    let value = List.Assoc.get_exn ~eq move moves in
    List.Assoc.update ~eq
      ~f:(function Some _ | None -> Some (Move.flip value))
      Movekey.Middle moves
    |> List.Assoc.update ~eq ~f:(function Some _ | None -> Some middle) move

  let apply transitions gs =
    let open Transition in
    match transitions with
    | Some (State state) -> State.{ gs with state }
    | Some (Do_move { move; src; dst }) ->
        let eq = Tile.Coord.equal in
        let ent = List.Assoc.get ~eq src gs.ents in
        let moves = advance move gs.moves in
        let ents =
          List.Assoc.update ~eq
            ~f:(function Some _ -> ent | None -> ent)
            dst gs.ents
          |> List.Assoc.remove ~eq src
        in
        { curr_team = Team.flip gs.curr_team; state = Choose_move; ents; moves }
    | Some (Restart team) -> init team 0
    | None -> gs
end
