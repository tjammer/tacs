(* TODO instead  using color have it like me and them *)
open Containers
open Game
open Moves
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))
module ImMoves = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Movekey))

let ( let* ) = Option.bind

let width = 1280

let height = 720

let line_thickness = 4

let layout = { Tile.size = { x = 100; y = 100 }; origin = { x = 200; y = 100 } }

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
      draw_rectangle x y (layout.size.x - 2) (layout.size.y - 2) color
    done
  done

let layout_blue =
  [
    ( Movekey.Blue_left,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 450 } } );
    ( Blue_right,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 450 } } );
  ]

let layout_red =
  [
    ( Movekey.Red_left,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 100 } } );
    ( Red_right,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 100 } } );
  ]

let layout_mid =
  Tile.{ size = { x = 30; y = 30 }; origin = { x = 900; y = 275 } }

module Easings = struct
  type t = Linear | Cubic | Quad

  let linear ~t ~start ~final ~dur = ((final -. start) *. t /. dur) +. start

  let cubic_in ~t ~start ~final ~dur =
    let t = t /. dur in
    ((final -. start) *. t *. t *. t) +. start

  let quad_in_out ~t ~start ~final ~dur =
    let final = final -. start in
    let t = t /. dur *. 2.0 in
    if t <. 1.0 then (final /. 2.0 *. (t *. t)) +. start
    else (-.final /. 2.0 *. (((t -. 1.0) *. (t -. 3.0)) -. 1.0)) +. start
end

let anim_dur = 0.25

type ent_anim = { src : Tile.Coord.t; t : float; x : float; y : float }

type move_anim = { src : float * float; t : float; layout : Tile.layout }

(* TODO this should not be here, instead a selection of mb left right?
 * or an input functor which does stuff depending on layout *)
let choose_move team mx my =
  let is_inside layout =
    let c = Tile.Coord.of_px mx my layout in
    inside_field c
  in
  List.find_map
    (fun (key, layout) -> if is_inside layout then Some key else None)
    (match team with Team.Blue -> layout_blue | Red -> layout_red)

let setup () =
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  let open Raylib in
  init_window 1280 720 "tacs";
  set_target_fps 60;
  let pawn_blue = load_texture "assets/blue_pawn.png" in
  let king_blue = load_texture "assets/blue_king.png" in
  let pawn_red = load_texture "assets/red_pawn.png" in
  let king_red = load_texture "assets/red_king.png" in

  let tbl, moves = restart Team.Blue in

  let ent_anims = Coordtbl.create 10 in

  ((pawn_blue, king_blue, pawn_red, king_red), ent_anims, tbl, moves)

let rec loop texs ent_anims tbl (state, team) (moves : Move.t ImMoves.t) =
  let open Raylib in
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let mpos = get_mouse_position () in
      let mx, my = Vector2.(x mpos, y mpos) in

      let input =
        let coord = Tile.Coord.of_px mx my layout in
        if is_mouse_button_pressed MouseButton.Left then Some (Select coord)
        else if is_mouse_button_pressed MouseButton.Right then Some Deselect
        else None
      in

      let transition =
        let* input = input in
        match (input, state) with
        | Select _, Choose_move ->
            let* move = choose_move team mx my in
            Some (State (Choose_ent move))
        | Select coord, Choose_ent move ->
            let* _ = choose_ent tbl coord team in
            Some (State (Move (move, coord)))
        | Select coord, Move (move, src) -> (
            let module Coordtbl = Hashtbl.Make (Tile.Coord) in
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
      in

      (* TODO gameplay state apply *)
      (* NOTE the hashtbl is mutated here *)
      let state, team, moves =
        match transition with
        | Some (State s) -> (s, team, moves)
        | Some (Do_move { move; src; dst }) ->
            let module Coordtbl = Hashtbl.Make (Tile.Coord) in
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
      in

      (* transition renderstat *)
      (let module Coordtbl = Hashtbl.Make (Tile.Coord) in
      match transition with
      | Some (Do_move { move = _; src; dst }) ->
          Coordtbl.replace ent_anims dst { src; t = 0.0; x = 0.0; y = 0.0 };
          (* TODO death anim *)
          Coordtbl.remove ent_anims src
      | Some (Restart _) -> Coordtbl.clear ent_anims
      | _ -> ());

      (* update renderstat *)
      (let module Coordtbl = Hashtbl.Make (Tile.Coord) in
      let fcoords coord layout =
        Tile.Coord.to_px coord layout |> Pair.map_same float_of_int
      in
      Coordtbl.filter_map_inplace
        (fun coord { src; t; x = _; y = _ } ->
          if t >=. anim_dur then None
          else
            let open Easings in
            let sx, sy = fcoords src layout in
            let fx, fy = fcoords coord layout in
            let x = quad_in_out ~t ~start:sx ~final:fx ~dur:anim_dur in
            let y = quad_in_out ~t ~start:sy ~final:fy ~dur:anim_dur in
            Some { src; t = t +. (1.0 /. 60.0); x; y })
        ent_anims);

      let highlights =
        match state with
        | Move (move, selected) -> (
            match ImMoves.find_opt moves move with
            | Some move ->
                List.filter_map
                  (fun mv ->
                    let target = Tile.Coord.add selected mv in
                    match
                      outcome_of_selected tbl (move, selected) team target
                    with
                    | Some `Take -> Some (target, `Take)
                    | Some `Win_move | Some `Win_take -> Some (target, `Win)
                    | Some `Move -> Some (target, `Move)
                    | None -> None)
                  move
            | None -> [] )
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

      (* draw only ents which are not animated *)
      ImCoords.iter
        (fun pos (kind, team) ->
          let tex =
            match (kind, team) with
            | Pawn, Team.Blue -> pawn_blue
            | King, Blue -> king_blue
            | Pawn, Red -> pawn_red
            | King, Red -> king_red
          in
          let x, y =
            match ImCoords.find_opt ent_anims pos with
            | None -> Tile.Coord.to_px pos layout |> Pair.map_same Float.of_int
            | Some { src = _; t = _; x; y } -> (x, y)
          in
          draw_texture_ex tex (Vector2.create x y) 0.0 scale Color.white)
        tbl;

      List.iter
        (fun (move, layout) ->
          let move = ImMoves.find_opt moves move |> Option.get_exn in
          draw_move move layout Color.skyblue)
        layout_blue;
      List.iter
        (fun (move, layout) ->
          let move = ImMoves.find_opt moves move |> Option.get_exn in
          draw_move move layout Color.orange)
        layout_red;
      draw_move
        (ImMoves.find_opt moves Movekey.Middle |> Option.get_exn)
        layout_mid
        (match team with Blue -> Color.skyblue | Red -> Color.orange);

      let highlight_ent ent =
        let x, y = Tile.Coord.to_px ent layout in
        draw_rectangle_lines_ex
          (recti (x + line_thickness) (y + line_thickness)
             (layout.size.x - (2 * line_thickness))
             (layout.size.y - (2 * line_thickness)))
          line_thickness Color.yellow
      in

      let highlight_move move team =
        let layout =
          (match team with Team.Blue -> layout_blue | Red -> layout_red)
          |> List.Assoc.get_exn move ~eq:Movekey.equal
        in
        draw_rectangle_lines_ex
          (recti layout.origin.x layout.origin.y (layout.size.x * 5)
             (layout.size.y * 5))
          line_thickness Color.yellow;
        ()
      in
      ( match state with
      | Choose_move -> ()
      | Choose_ent move -> highlight_move move team
      | Move (move, selected) ->
          highlight_move move team;
          highlight_ent selected
      | Over k ->
          draw_text
            ("Game Over! " ^ Team.to_str (Team.flip k) ^ " won!")
            100
            ((height / 2) - 100)
            100 Color.black );

      end_drawing ();
      loop texs ent_anims tbl (state, team) moves

let () =
  let texs, ent_anims, tbl, moves = setup () in
  loop texs ent_anims tbl (Choose_move, Blue) moves
