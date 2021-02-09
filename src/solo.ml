open ContainersLabels

let () = Random.self_init ()

let rst = Random.get_state ()

let def_ai =
  Ai.
    {
      move = Game.Moves.Movekey.Middle;
      ent = { Game.Tile.x = 0; y = 0 };
      coord = { Game.Tile.x = 0; y = 0 };
    }

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
          loop clientstate gamestate def_ai
      | _ ->
          begin_drawing ();
          clear_background Color.raywhite;
          Client.draw clientstate gamestate;
          end_drawing ();

          if is_key_pressed Key.Escape then Lwt.return `Back
          else game_over clientstate gamestate )

and loop clientstate gamestate ai =
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
          match gamestate.state with
          | Choose_move ->
              let ai = Ai.best_move 3 gamestate in
              let move =
                match Ai.(ai.move) with
                | Game.Moves.Movekey.Blue_left | Red_left -> Game.Input.Left
                | Blue_right | Red_right -> Right
                | Middle -> assert false
              in
              (Some (Select (`Move move)), ai)
          | Choose_ent _ -> (Some (Select (`Ent ai.ent)), ai)
          | Move _ -> (Some (Select (`Ent ai.coord)), ai)
          | Over _ -> (Some (Select (`Move Left)), ai)
      in

      let trans = Game.transitions input gamestate in

      let gamestate = Game.Mut.apply trans gamestate in

      Client.Mut.apply clientstate trans;
      Client.Mut.update_renderstate clientstate;

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;
      end_drawing ();

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
    def_ai
