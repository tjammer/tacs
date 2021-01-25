open Containers
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))
module ImMoves = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Moves.Movekey))

type ent_anim = {
  src : float * float;
  t : float;
  x : float;
  y : float;
  dur : float;
}

type move_anim = {
  src : float * float;
  t : float;
  layout : Tile.layout;
  dur : float;
}

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

let line_thickness = 4

let layout = { Tile.size = { x = 100; y = 100 }; origin = { x = 200; y = 100 } }

let move_layouts =
  [
    ( Moves.Movekey.Blue_left,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 450 } } );
    ( Blue_right,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 450 } } );
    ( Moves.Movekey.Red_left,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 100 } } );
    ( Red_right,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 100 } } );
    ( Moves.Movekey.Middle,
      Tile.{ size = { x = 30; y = 30 }; origin = { x = 900; y = 275 } } );
  ]

let get_move_input team mx my =
  let is_inside layout =
    let c = Tile.Coord.of_px mx my layout in
    Moves.inside_field c
  in
  move_layouts
  |> List.filter (fun (key, _) ->
         match (team, key) with
         | Game.Team.Blue, Moves.Movekey.Blue_left | Blue, Blue_right -> true
         | Red, Red_left | Red, Red_right -> true
         | _ -> false)
  |> List.find_map (fun (key, layout) ->
         if is_inside layout then
           let input =
             match key with
             | Moves.Movekey.Blue_left | Red_left -> Some Game.Input.Left
             | Blue_right | Red_right -> Some Right
             | Middle -> None
           in
           input
         else None)

let input team =
  let open Raylib in
  let mpos = get_mouse_position () in
  let mx, my = Vector2.(x mpos, y mpos) in
  if is_mouse_button_pressed MouseButton.Left then
    let select =
      match get_move_input team mx my with
      | Some inp -> `Move inp
      | None -> `Ent (Tile.Coord.of_px mx my layout)
    in
    Some (Game.Input.Select select)
  else if is_mouse_button_pressed MouseButton.Right then Some Deselect
  else None

module Mut = struct
  module Coordtbl = Hashtbl.Make (Tile.Coord)
  module Movetbl = Hashtbl.Make (Moves.Movekey)

  let transitions ent_anims move_anims transitions =
    let open Game.Transition in
    match transitions with
    | Some (Do_move { move; src; dst }) ->
        let ent_anim =
          {
            src = Tile.Coord.to_pxf src layout;
            t = 0.0;
            x = 0.0;
            y = 0.0;
            dur = 0.25;
          }
        in
        Coordtbl.replace ent_anims dst ent_anim;

        (* TODO death anim *)
        Coordtbl.remove ent_anims src;

        let eq = Moves.Movekey.equal in
        let layout = (List.Assoc.get_exn ~eq move) move_layouts in
        let middle = (List.Assoc.get_exn ~eq Middle) move_layouts in
        let x, y = Tile.Coord.to_pxf { x = 0; y = 0 } middle in
        let dur = 0.3 in
        Movetbl.replace move_anims move
          { src = (x, y); t = 0.0; layout = middle; dur };
        let x, y = Tile.Coord.to_pxf { x = 0; y = 0 } layout in
        Movetbl.replace move_anims Middle { src = (x, y); t = 0.0; layout; dur }
    | Some (Restart _) -> Coordtbl.clear ent_anims
    | _ -> ()

  let update_renderstate ent_anims move_anims =
    Coordtbl.filter_map_inplace
      (fun coord { src = sx, sy; t; x = _; y = _; dur } ->
        if t >=. dur then None
        else
          let open Easings in
          let fx, fy = Tile.Coord.to_pxf coord layout in
          let x = quad_in_out ~t ~start:sx ~final:fx ~dur in
          let y = quad_in_out ~t ~start:sy ~final:fy ~dur in
          Some { src = (sx, sy); t = t +. (1.0 /. 60.0); x; y; dur })
      ent_anims;

    Movetbl.filter_map_inplace
      (fun move { src = sx, sy; t; layout = _; dur } ->
        if t >=. dur then None
        else
          let eq = Moves.Movekey.equal in
          let open Easings in
          let layout = (List.Assoc.get_exn ~eq move) move_layouts in
          let fx, fy = Tile.Coord.to_pxf { x = 0; y = 0 } layout in
          let x = quad_in_out ~t ~start:sx ~final:fx ~dur in
          let y = cubic_in ~t ~start:sy ~final:fy ~dur in
          let layout =
            { layout with origin = { x = int_of_float x; y = int_of_float y } }
          in
          Some { src = (sx, sy); t = t +. (1.0 /. 60.0); layout; dur })
      move_anims
end

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

let draw texs ent_anims move_anims tbl state team moves =
  let open Raylib in
  let highlights =
    match state with
    | Game.Move (move, selected) -> (
        match ImMoves.find_opt moves move with
        | Some move ->
            List.filter_map
              (fun mv ->
                let target = Tile.Coord.add selected mv in
                match
                  Game.outcome_of_selected tbl (move, selected) team target
                with
                | Some `Take -> Some (target, `Take)
                | Some `Win_move | Some `Win_take -> Some (target, `Win)
                | Some `Move -> Some (target, `Move)
                | None -> None)
              move
        | None -> [] )
    | _ -> []
  in

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
        | Game.(Pawn, Team.Blue) -> pawn_blue
        | King, Blue -> king_blue
        | Pawn, Red -> pawn_red
        | King, Red -> king_red
      in
      let x, y =
        match ImCoords.find_opt ent_anims pos with
        | None -> Tile.Coord.to_px pos layout |> Pair.map_same Float.of_int
        | Some anim -> (anim.x, anim.y)
      in
      draw_texture_ex tex (Vector2.create x y) 0.0 scale Color.white)
    tbl;

  List.iter
    (fun (movekey, layout) ->
      let layout =
        match ImMoves.find_opt move_anims movekey with
        | Some anim -> anim.layout
        | None -> layout
      in
      let move = ImMoves.find_exn moves movekey in
      let color =
        match movekey with
        | Blue_left | Blue_right -> Color.skyblue
        | Red_left | Red_right -> Color.orange
        | Middle ->
            fade
              (match team with Blue -> Color.skyblue | Red -> Color.orange)
              0.5
      in
      draw_move move layout color)
    move_layouts;

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
      move_layouts |> List.Assoc.get_exn move ~eq:Moves.Movekey.equal
    in
    draw_rectangle_lines_ex
      (recti layout.origin.x layout.origin.y (layout.size.x * 5)
         (layout.size.y * 5))
      line_thickness Color.yellow;
    ()
  in
  match state with
  | Choose_move -> ()
  | Choose_ent move -> highlight_move move
  | Move (move, selected) ->
      highlight_move move;
      highlight_ent selected
  | Over k ->
      draw_text
        ("Game Over! " ^ Game.(Team.to_str (Team.flip k)) ^ " won!")
        100
        ((get_screen_height () / 2) - 100)
        100 Color.black
