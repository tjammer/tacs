open ContainersLabels
module Moves = Game.Moves
module Tile = Game.Tile
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))
module ImMoves = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Moves.Movekey))
module Bar = Helpers.Bar
module Button = Helpers.Button

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

module State = struct
  type tex = Raylib.Texture2D.t

  type t = {
    texs : tex * tex * tex * tex;
    layout : Game.Tile.layout;
    move_layouts : (Moves.Movekey.t * Tile.layout) list;
    pov_team : Game.Team.t;
    ent_anims : ent_anim ImCoords.t;
    move_anims : move_anim ImMoves.t;
  }
end

let line_thickness = 4

let get_move_input cs team mx my =
  let is_inside layout =
    let c = Tile.Coord.of_px mx my layout in
    Moves.inside_field c
  in
  State.(cs.move_layouts)
  |> List.filter ~f:(fun (key, _) ->
         match (team, key) with
         | Game.Team.Blue, Moves.Movekey.Blue_left | Blue, Blue_right -> true
         | Red, Red_left | Red, Red_right -> true
         | _ -> false)
  |> List.find_map ~f:(fun (key, layout) ->
         if is_inside layout then
           let input =
             match key with
             | Moves.Movekey.Blue_left | Red_left -> Some Game.Input.Left
             | Blue_right | Red_right -> Some Right
             | Middle -> None
           in
           input
         else None)

let input cs team =
  let open Raylib in
  let mpos = get_mouse_position () in
  let mx, my = Vector2.(x mpos, y mpos) in
  if is_mouse_button_pressed MouseButton.Left then
    let select =
      match get_move_input cs team mx my with
      | Some inp -> `Move inp
      | None -> `Ent (Tile.Coord.of_px mx my cs.layout)
    in
    Some (Game.Input.Select select)
  else if is_mouse_button_pressed MouseButton.Right then Some Deselect
  else None

module Mut = struct
  module Coordtbl = Hashtbl.Make (Tile.Coord)
  module Movetbl = Hashtbl.Make (Moves.Movekey)

  let apply cs transitions =
    let open Game.Transition in
    match transitions with
    | Some (Do_move { move; src; dst }) ->
        let ent_anim =
          {
            src = Tile.Coord.to_pxf src State.(cs.layout);
            t = 0.0;
            x = 0.0;
            y = 0.0;
            dur = 0.25;
          }
        in
        Coordtbl.replace State.(cs.ent_anims) dst ent_anim;

        (* TODO death anim *)
        Coordtbl.remove cs.ent_anims src;

        let eq = Moves.Movekey.equal in
        let layout = (List.Assoc.get_exn ~eq move) cs.move_layouts in
        let middle = (List.Assoc.get_exn ~eq Middle) cs.move_layouts in
        let x, y = Tile.Coord.to_pxf { x = 0; y = 0 } middle in
        let dur = 0.3 in
        Movetbl.replace cs.move_anims move
          { src = (x, y); t = 0.0; layout = middle; dur };
        let x, y = Tile.Coord.to_pxf { x = 0; y = 0 } layout in
        Movetbl.replace cs.move_anims Middle
          { src = (x, y); t = 0.0; layout; dur }
    | Some (Restart _) -> Coordtbl.clear cs.ent_anims
    | _ -> ()

  let t = ref (Sys.time ())

  let update_renderstate cs =
    let t' = Sys.time () in
    let dt = Float.max (1.0 /. 60.0) (t' -. !t) in
    t := t';

    Coordtbl.filter_map_inplace
      (fun coord { src = sx, sy; t; x = _; y = _; dur } ->
        if t >=. dur then None
        else
          let open Helpers.Easings in
          let fx, fy = Tile.Coord.to_pxf coord State.(cs.layout) in
          let x = quad_in_out ~t ~start:sx ~final:fx ~dur in
          let y = quad_in_out ~t ~start:sy ~final:fy ~dur in
          Some { src = (sx, sy); t = t +. dt; x; y; dur })
      State.(cs.ent_anims);

    Movetbl.filter_map_inplace
      (fun move { src = sx, sy; t; layout = _; dur } ->
        if t >=. dur then None
        else
          let eq = Moves.Movekey.equal in
          let open Helpers.Easings in
          let layout = (List.Assoc.get_exn ~eq move) cs.move_layouts in
          let fx, fy = Tile.Coord.to_pxf { x = 0; y = 0 } layout in
          let x = quad_in_out ~t ~start:sx ~final:fx ~dur in
          let y = cubic_in ~t ~start:sy ~final:fy ~dur in
          let layout =
            { layout with origin = { x = int_of_float x; y = int_of_float y } }
          in
          Some { src = (sx, sy); t = t +. dt; layout; dur })
      cs.move_anims
end

let recti x y width height =
  Raylib.Rectangle.create (Float.of_int x) (Float.of_int y) (Float.of_int width)
    (Float.of_int height)

let draw_move move layout hl =
  let border = 3 in
  let open Raylib in
  for y = 0 to 4 do
    for x = 0 to 4 do
      let color =
        if x = 2 && y = 2 then Color.gray
        else
          List.fold_while
            ~f:(fun acc coord ->
              if Tile.Coord.equal coord { x = x - 2; y = y - 2 } then (hl, `Stop)
              else (acc, `Continue))
            ~init:Color.lightgray move
      in
      let x, y = Tile.Coord.to_px { x; y } layout in
      draw_rectangle x y (layout.size.x - border) (layout.size.y - border) color
    done
  done

let draw cs gs =
  let open Raylib in
  (* we need the abs due to negative values *)
  let lsx = abs State.(cs.layout).size.x in
  let lsy = abs cs.layout.size.y in

  let highlights =
    match Game.State.(gs.state) with
    | Game.Move (move, selected) -> (
        match List.Assoc.get ~eq:Moves.Movekey.equal move gs.moves with
        | Some move ->
            List.filter_map
              ~f:(fun mv ->
                let target = Tile.Coord.add selected mv in
                match Game.outcome_of_selected gs (move, selected) target with
                | Some Take -> Some (target, `Take)
                | Some Win_move | Some Win_take -> Some (target, `Win)
                | Some Move -> Some (target, `Move)
                | None -> None)
              move
        | None -> [])
    | _ -> []
  in

  List.iter
    ~f:(fun (x, y) ->
      let x, y = Tile.Coord.to_px { x; y } cs.layout in
      draw_rectangle (x + line_thickness) (y + line_thickness)
        (lsx - (2 * line_thickness))
        (lsy - (2 * line_thickness))
        (fade Color.lightgray 0.25))
    [ (2, 0); (2, 4) ];

  for y = 0 to 4 do
    for x = 0 to 4 do
      let x, y = Tile.Coord.to_px { x; y } cs.layout in
      draw_rectangle_lines_ex
        (recti (x + line_thickness) (y + line_thickness)
           (lsx - (2 * line_thickness))
           (lsy - (2 * line_thickness)))
        line_thickness Color.lightgray
    done
  done;

  List.iter
    ~f:(fun (coord, mv_kind) ->
      let x, y = Tile.Coord.to_px coord cs.layout in
      let col =
        match mv_kind with
        | `Take -> Color.red
        | `Move -> Color.lime
        | `Win -> Color.yellow
      in
      draw_rectangle x y lsx lsy (fade col 0.5))
    highlights;

  let pawn_blue, king_blue, pawn_red, king_red = State.(cs.texs) in
  let scale =
    (cs.layout.size.x |> abs |> Float.of_int)
    /. Float.of_int (Texture2D.width pawn_blue)
  in

  (* draw only ents which are not animated *)
  List.iter
    ~f:(fun (pos, (kind, team)) ->
      let tex =
        match (kind, team) with
        | Game.(Pawn, Team.Blue) -> pawn_blue
        | King, Blue -> king_blue
        | Pawn, Red -> pawn_red
        | King, Red -> king_red
      in
      let x, y =
        match ImCoords.find_opt cs.ent_anims pos with
        | None -> Tile.Coord.to_pxf pos cs.layout
        | Some anim -> (anim.x, anim.y)
      in
      draw_texture_ex tex (Vector2.create x y) 0.0 scale Color.white)
    gs.ents;

  List.iter
    ~f:(fun (movekey, layout) ->
      let layout =
        match ImMoves.find_opt cs.move_anims movekey with
        | Some anim -> anim.layout
        | None -> layout
      in
      let move =
        List.Assoc.get_exn ~eq:Moves.Movekey.equal movekey gs.moves
        |> fun move ->
        match cs.pov_team with Blue -> move | Red -> Moves.Move.flip move
      in
      let color =
        match movekey with
        | Blue_left | Blue_right -> Color.skyblue
        | Red_left | Red_right -> Color.orange
        | Middle ->
            fade
              (match gs.curr_team with
              | Blue -> Color.skyblue
              | Red -> Color.orange)
              0.5
      in
      draw_move move layout color)
    cs.move_layouts;

  let highlight_ent ent =
    let x, y = Tile.Coord.to_px ent cs.layout in
    draw_rectangle_lines_ex
      (recti (x + line_thickness) (y + line_thickness)
         (lsx - (2 * line_thickness))
         (lsy - (2 * line_thickness)))
      line_thickness Color.yellow
  in

  let highlight_move move =
    let layout =
      cs.move_layouts |> List.Assoc.get_exn move ~eq:Moves.Movekey.equal
    in
    draw_rectangle_lines_ex
      (recti
         (layout.origin.x - line_thickness)
         (layout.origin.y - line_thickness)
         ((layout.size.x * 5) + line_thickness)
         ((layout.size.y * 5) + line_thickness))
      line_thickness Color.yellow;
    ()
  in
  match gs.state with
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

let pawn_blue = lazy (Raylib.load_texture "assets/blue_pawn.png")

let king_blue = lazy (Raylib.load_texture "assets/blue_king.png")

let pawn_red = lazy (Raylib.load_texture "assets/red_pawn.png")

let king_red = lazy (Raylib.load_texture "assets/red_king.png")

let init_state pov_team =
  (* TODO implemend this properly *)
  let layout =
    match pov_team with
    | Game.Team.Blue ->
        { Tile.size = { x = 100; y = 100 }; origin = { x = 200; y = 100 } }
    | Red ->
        { Tile.size = { x = -100; y = -100 }; origin = { x = 700; y = 600 } }
  in

  let move_layouts =
    match pov_team with
    | Blue ->
        [
          ( Moves.Movekey.Blue_left,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 450 } } );
          ( Blue_right,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 450 } }
          );
          ( Moves.Movekey.Red_left,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 100 } } );
          ( Red_right,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 100 } }
          );
          ( Moves.Movekey.Middle,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 900; y = 275 } } );
        ]
    | Red ->
        [
          ( Moves.Movekey.Red_left,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 450 } } );
          ( Red_right,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 450 } }
          );
          ( Moves.Movekey.Blue_left,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 800; y = 100 } } );
          ( Blue_right,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 1000; y = 100 } }
          );
          ( Moves.Movekey.Middle,
            Tile.{ size = { x = 30; y = 30 }; origin = { x = 900; y = 275 } } );
        ]
  in

  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  let module Movetbl = Hashtbl.Make (Moves.Movekey) in
  let texs =
    Lazy.(force pawn_blue, force king_blue, force pawn_red, force king_red)
  in

  let ent_anims = Coordtbl.create 10 in
  let move_anims = Movetbl.create 5 in
  State.{ texs; ent_anims; move_anims; layout; move_layouts; pov_team }
