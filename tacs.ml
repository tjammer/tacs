(* TODO instead  using color have it like me and them *)
open Containers
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))

let ( let* ) = Option.bind

let width = 1280

let height = 720

let line_thickness = 4

let layout = { Tile.size = { x = 100; y = 100 }; origin = { x = 200; y = 100 } }

type kind = Pawn | King [@@deriving eq, show]

module Team = struct
  type t = Blue | Red [@@deriving eq]

  let flip = function Blue -> Red | Red -> Blue

  let to_str = function Blue -> "Blue" | Red -> "Red"

  let win_field = function
    | Blue -> Tile.{ x = 2; y = 0 }
    | Red -> { x = 2; y = 4 }
end

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

  let key_of_team = function Team.Blue -> Blue | Team.Red -> Red
end

let mantis = [ { Tile.x = 0; y = -1 }; { x = -1; y = 1 }; { x = 1; y = 1 } ]

let horse = [ { Tile.x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } ]

let tiger = [ { Tile.x = 0; y = 1 }; { x = 0; y = -2 } ]

let ox = [ { Tile.x = 0; y = -1 }; { x = 1; y = 0 }; { x = 0; y = 1 } ]

let cobra = [ { Tile.x = 1; y = 1 }; { x = 1; y = -1 }; { x = -1; y = 0 } ]

let monkey =
  [
    { Tile.x = -1; y = -1 };
    { x = -1; y = 1 };
    { x = 1; y = 1 };
    { x = 1; y = -1 };
  ]

let all_moves = [ mantis; horse; tiger; ox; cobra; monkey ]

type state =
  | Choose_move
  | Choose_ent of Move.t
  | Move of Move.t * Tile.Coord.t * kind
  | Over of Team.t
  | Transition of [ `Switch of Move.t | `Restart ]

let choose_ent tbl coord team =
  match ImCoords.find_opt tbl coord with
  | Some (kind, t) -> if Team.equal team t then Some kind else None
  | None -> None

let outcome_of_selected tbl (move, selected, kind) team dst =
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

let recti x y width height =
  Raylib.Rectangle.create (Float.of_int x) (Float.of_int y) (Float.of_int width)
    (Float.of_int height)

let draw_move move layout hl =
  let open Raylib in
  for y = 0 to 4 do
    for x = 0 to 4 do
      let color =
        if x = 2 && y = 2 then Color.gray
        else
          List.fold_while
            (fun acc coord ->
              if Tile.Coord.equal coord { x = x - 2; y = y - 2 } then (hl, `Stop)
              else (acc, `Continue))
            Color.lightgray move
      in
      let x, y = Tile.Coord.to_px { x; y } layout in
      draw_rectangle x y (layout.size.x - 1) (layout.size.y - 1) color
    done
  done

let layout_blue =
  [
    Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 450 } };
    Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 450 } };
  ]

let layout_red =
  [
    Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 100 } };
    Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 100 } };
  ]

let layout_mid =
  Tile.{ size = { x = 30; y = 30 }; origin = { x = 900; y = 275 } }

let choose_move moves team mx my =
  let is_inside layout =
    let c = Tile.Coord.of_px mx my layout in
    inside_field c
  in
  match team with
  | Team.Blue ->
      List.find_mapi
        (fun i layout ->
          if is_inside layout then Some (List.nth Moves.(get_all moves Blue) i)
          else None)
        layout_blue
  | Red ->
      List.find_mapi
        (fun i layout ->
          if is_inside layout then Some (List.nth Moves.(get_all moves Red) i)
          else None)
        layout_red

let advance move team moves =
  let team = Moves.key_of_team team in
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

let restart () =
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

  let moves = Moves.distribute_initial all_moves in
  (tbl, moves)

let setup () =
  let open Raylib in
  init_window 1280 720 "tacs";
  set_target_fps 60;
  let pawn_blue = load_texture "assets/blue_pawn.png" in
  let king_blue = load_texture "assets/blue_king.png" in
  let pawn_red = load_texture "assets/red_pawn.png" in
  let king_red = load_texture "assets/red_king.png" in

  let tbl, moves = restart () in

  ((pawn_blue, king_blue, pawn_red, king_red), tbl, moves)

let rec loop texs tbl (state, team) (moves : Moves.t) =
  let open Raylib in
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let mpos = get_mouse_position () in
      let mx, my = Vector2.(x mpos, y mpos) in
      let coord = Tile.Coord.of_px mx my layout in

      let state =
        if is_mouse_button_pressed MouseButton.Left then
          match state with
          | Choose_move -> (
              match choose_move moves team mx my with
              | Some move -> Choose_ent move
              | None -> Choose_move )
          | Choose_ent move -> (
              match choose_ent tbl coord team with
              | Some kind -> Move (move, coord, kind)
              | None -> state )
          | Move (m, selected, kind) -> (
              let module Coordtbl = Hashtbl.Make (Tile.Coord) in
              match outcome_of_selected tbl (m, selected, kind) team coord with
              | Some `Take ->
                  Coordtbl.replace tbl coord (kind, team);
                  Coordtbl.remove tbl selected;
                  Transition (`Switch m)
              | Some `Move ->
                  Coordtbl.add tbl coord (kind, team);
                  Coordtbl.remove tbl selected;
                  Transition (`Switch m)
              | Some `Win_take | Some `Win_move -> Over team
              | None -> Move (m, selected, kind) )
          | Over k -> Over k
          | Transition t -> Transition t
        else if is_mouse_button_pressed MouseButton.Right then
          match state with
          | Choose_move -> Choose_move
          | Choose_ent _ -> Choose_move
          | Move (move, _, _) -> Choose_ent move
          | Over _ -> Transition `Restart
          | Transition t -> Transition t
        else state
      in

      ( match state with
      | Transition t -> (
          match t with
          | `Switch move ->
              let moves = advance move team moves in
              loop texs tbl (Choose_move, Team.flip team) moves
          | `Restart ->
              let tbl, moves = restart () in
              loop texs tbl (Choose_move, Team.flip team) moves )
      | _ -> () );

      let highlights =
        match state with
        | Move (move, selected, kind) ->
            List.filter_map
              (fun mv ->
                let target = Tile.Coord.add selected mv in
                match
                  outcome_of_selected tbl (move, selected, kind) team target
                with
                | Some `Take -> Some (target, `Take)
                | Some `Win_move | Some `Win_take -> Some (target, `Win)
                | Some `Move -> Some (target, `Move)
                | None -> None)
              move
        | _ -> []
      in

      begin_drawing ();
      clear_background Color.raywhite;

      for y = 0 to 4 do
        for x = 0 to 4 do
          let x, y = Tile.Coord.to_px { x; y } layout in
          draw_rectangle_lines_ex
            (recti (x + line_thickness) (y + line_thickness)
               (layout.size.x - (2 * line_thickness))
               (layout.size.y - (2 * line_thickness)))
            line_thickness Color.lightgray
        done
      done;

      List.iter
        (fun (coord, mv_kind) ->
          let x, y = Tile.Coord.to_px coord layout in
          let col =
            match mv_kind with
            | `Take -> Color.red
            | `Move -> Color.lime
            | `Win -> Color.yellow
          in
          draw_rectangle x y layout.size.x layout.size.y (fade col 0.5))
        highlights;

      let pawn_blue, king_blue, pawn_red, king_red = texs in
      let scale =
        Float.of_int layout.size.x /. Float.of_int (Texture.width pawn_blue)
      in
      ImCoords.iter
        (fun pos ent ->
          let tex =
            match ent with
            | Pawn, Team.Blue -> pawn_blue
            | King, Blue -> king_blue
            | Pawn, Red -> pawn_red
            | King, Red -> king_red
          in
          let x, y = Tile.Coord.to_px pos layout in
          draw_texture_ex tex
            (Vector2.create (Float.of_int x) (Float.of_int y))
            0.0 scale Color.white)
        tbl;

      List.iter2
        (fun move layout -> draw_move move layout Color.skyblue)
        (Moves.get_all moves Blue) layout_blue;

      List.iter2
        (fun move layout -> draw_move move layout Color.orange)
        (Moves.get_all moves Red) layout_red;
      draw_move (Moves.get_middle moves) layout_mid
        (match team with Blue -> Color.skyblue | Red -> Color.orange);

      let highlight_ent ent =
        let x, y = Tile.Coord.to_px ent layout in
        draw_rectangle_lines_ex
          (recti (x + line_thickness) (y + line_thickness)
             (layout.size.x - (2 * line_thickness))
             (layout.size.y - (2 * line_thickness)))
          line_thickness Color.yellow
      in

      let highlight_move move =
        let layout =
          match
            List.combine (Moves.get_all moves Blue) layout_blue
            |> List.Assoc.get move ~eq:Move.equal
          with
          | Some layout -> layout
          | None ->
              List.combine (Moves.get_all moves Red) layout_red
              |> List.Assoc.get_exn move ~eq:Move.equal
        in

        draw_rectangle_lines_ex
          (recti layout.origin.x layout.origin.y (layout.size.x * 5)
             (layout.size.y * 5))
          line_thickness Color.yellow;
        ()
      in
      ( match state with
      | Choose_move -> ()
      | Choose_ent move -> highlight_move move
      | Move (move, selected, _) ->
          highlight_move move;
          highlight_ent selected
      | Over k ->
          draw_text
            ("Game Over! " ^ Team.to_str k ^ " won!")
            100
            ((height / 2) - 100)
            100 Color.black
      | Transition _ -> () );

      end_drawing ();
      loop texs tbl (state, team) moves

let () =
  let texs, tbl, moves = setup () in
  loop texs tbl (Choose_move, Blue) moves
