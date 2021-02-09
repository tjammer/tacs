open ContainersLabels

let () = Random.self_init ()

let rst = Random.get_state ()

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
      | _ ->
          begin_drawing ();
          clear_background Color.raywhite;
          Client.draw clientstate gamestate;
          end_drawing ();

          if is_key_pressed Key.Escape then Lwt.return `Back
          else game_over clientstate gamestate )

and loop clientstate gamestate =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      let input = Client.input clientstate Game.State.(gamestate.curr_team) in
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
        | _ -> loop clientstate gamestate )

let start () =
  let seed = Random.bits () in
  loop
    (Client.init_state (Random.pick_list [ Game.Team.Blue; Red ] rst))
    (Game.init Game.Team.Blue seed)
