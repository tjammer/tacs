open ContainersLabels
open Lwt.Infix

let () = Random.self_init ()

let rst = Random.get_state ()

let def_ai =
  Ai.
    {
      move = Game.Moves.Movekey.Middle;
      ent = { Game.Tile.x = 0; y = 0 };
      coord = { Game.Tile.x = 0; y = 0 };
    }

type ai = { seq : Ai.move_seq; fresh : bool }

let rec game_over clientstate gamestate =
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
            (Lwt.return { seq = def_ai; fresh = false })
      | _ ->
          begin_drawing ();
          clear_background Color.raywhite;
          Client.draw clientstate gamestate;
          end_drawing ();

          if is_key_pressed Key.Escape then Lwt.return `Back
          else game_over clientstate gamestate )

and loop clientstate gamestate (ai : ai Lwt.t) =
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
                    let seq = Ai.best_move 2 gamestate in
                    ( None,
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
              | Over _ -> (Some (Select (`Move Left)), ai) )
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
        | Game.Over _ -> game_over clientstate gamestate
        | _ -> loop clientstate gamestate ai )

let start () =
  let seed = Random.bits () in
  loop
    (Client.init_state (Random.pick_list [ Game.Team.Blue; Red ] rst))
    (Game.init Game.Team.Blue seed)
    (Lwt.return { seq = def_ai; fresh = false })
