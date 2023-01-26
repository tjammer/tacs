open ContainersLabels
open Lwt.Infix

let () = Random.self_init ()
let rst = Random.get_state ()

type ai = { seq : Ai.move_seq; fresh : bool }
type difficulty = Easy | Normal | Hard

let rec game_over clientstate gamestate difficulty =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      match Client.input clientstate Game.State.(gamestate.curr_team) with
      | Some (Select _) ->
          let seed = Random.bits () in
          let gamestate =
            Game.init Game.(Team.flip State.(gamestate.curr_team)) seed
          in
          loop clientstate gamestate
            (Lwt.return { seq = Ai.def_seq; fresh = false })
            difficulty
      | _ ->
          begin_drawing ();
          clear_background Color.raywhite;
          Client.draw clientstate gamestate;
          end_drawing ();

          if is_key_pressed Key.Escape then Lwt.return `Back
          else game_over clientstate gamestate difficulty)

and loop clientstate gamestate ai difficulty =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      let input, ai =
        if
          Game.(
            Team.equal
              State.(gamestate.curr_team)
              Client.State.(clientstate.pov_team))
        then (Client.input clientstate gamestate.curr_team, ai)
        else
          match Lwt.state ai with
          | Return { seq; fresh } -> (
              match gamestate.state with
              | Choose_move ->
                  if not fresh then
                    ( None,
                      Lwt_preemptive.detach (Ai.best_move difficulty) gamestate
                      >>= fun (seq, _) ->
                      Lwt_unix.sleep (Random.float_range 0.2 0.7 rst)
                      >>= fun () -> Lwt.return { seq; fresh = true } )
                  else
                    let move' =
                      match Ai.(seq.move) with
                      | Game.Moves.Movekey.Blue_left | Red_left ->
                          Game.Input.Left
                      | Blue_right | Red_right -> Right
                      | Middle -> assert false
                    in
                    ( Some (Select (`Move move')),
                      Lwt_unix.sleep (Random.float_range 0.4 0.7 rst)
                      >>= fun () -> Lwt.return { seq; fresh = false } )
              | Choose_ent _ ->
                  ( Some (Select (`Ent seq.ent)),
                    Lwt_unix.sleep (Random.float_range 0.7 1.4 rst)
                    >>= fun () -> ai )
              | Move _ ->
                  ( Some (Select (`Ent seq.coord)),
                    Lwt_unix.sleep (Random.float_range 0.5 1.0 rst)
                    >>= fun () -> ai )
              | Over _ -> (Some (Select (`Move Left)), ai))
          | Sleep -> (None, ai)
          | Fail _ -> assert false
      in

      let trans = Game.transitions input gamestate in

      let gamestate = Game.Mut.apply trans gamestate in

      Client.Mut.apply clientstate trans;
      Client.Mut.update_renderstate clientstate;

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;
      end_drawing ();

      Lwt.pause () >>= fun () ->
      if is_key_pressed Key.Escape then Lwt.return `Back
      else
        match gamestate.state with
        | Game.Over _ -> game_over clientstate gamestate difficulty
        | _ -> loop clientstate gamestate ai difficulty)

let start difficulty () =
  let seed = Random.bits () in
  loop
    (Client.init_state (Random.pick_list [ Game.Team.Blue; Red ] rst))
    (Game.init Game.Team.Blue seed)
    (Lwt.return { seq = Ai.def_seq; fresh = false })
    difficulty

let rec loop_select_diff buttons width =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      let mx, my = (get_mouse_x (), get_mouse_y ()) in
      let buttons = List.map ~f:(Client.Button.update mx my) buttons in

      match
        if is_mouse_button_pressed MouseButton.Left then
          Client.Button.on_click buttons (function
            | Easy -> Some (start 1)
            | Normal -> Some (start 2)
            | Hard -> Some (start 4))
        else None
      with
      | Some f -> f ()
      | None ->
          begin_drawing ();
          clear_background Color.raywhite;

          List.iter
            ~f:(fun but ->
              let x, y = Client.Button.xy but in
              Client.Bar.draw_text but.bar x y
                (match but.mode with
                | Normal -> "Normal"
                | Hard -> "Hard"
                | Easy -> "Easy")
                50)
            buttons;

          let txt = "TACS" in
          let sz = 100 in
          let w = measure_text txt sz in
          let x = (width * 2 / 3) - (w / 6) in
          draw_text txt x 175 sz Color.gray;

          end_drawing ();
          if is_key_pressed Key.Escape then Lwt.return `Back
          else loop_select_diff buttons width)

let select (width, height) () =
  (* we draw one frame to reset the mouse state *)
  let open Raylib in
  let buttons =
    Client.(
      Button.layout Bar.bar
        (List.map2 ~f:Pair.make Bar.bars [ Easy; Normal; Hard ])
        80 0 (width / 2) height)
  in

  begin_drawing ();
  clear_background Color.raywhite;

  List.iter
    ~f:(fun but ->
      let x, y = Client.Button.xy but in
      Client.Bar.draw_text but.bar x y
        (match but.mode with
        | Normal -> "Normal"
        | Hard -> "Hard"
        | Easy -> "Easy")
        50)
    buttons;

  let txt = "TACS" in
  let sz = 100 in
  let w = measure_text txt sz in
  let x = (width * 2 / 3) - (w / 6) in
  draw_text txt x 175 sz Color.gray;
  end_drawing ();

  loop_select_diff buttons width
