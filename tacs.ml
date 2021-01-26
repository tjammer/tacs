let ( let* ) = Lwt.bind

let width = 1280

let height = 720

let tic2 () =
  let* () = Lwt_unix.sleep 1.0 in
  Lwt_io.printl "tix%!"

let setup team =
  let open Raylib in
  init_window 1280 720 "tacs";
  set_target_fps 60;

  let clientstate = Client.init_state () in
  let gamestate = Game.(restart team) in

  (clientstate, gamestate)

let rec loop clientstate (gamestate : Game.State.t) tc =
  let open Raylib in
  match window_should_close () with
  | true ->
      close_window ();
      Lwt.return_unit
  | false ->
      let tc =
        match Lwt.state tc with
        | Sleep -> tc
        | Return _ -> tic2 ()
        | Fail _ -> tic2 ()
      in

      let input = Client.input gamestate.curr_team in
      let trans = Game.transitions input gamestate in

      let gamestate = Game.Mut.apply trans gamestate in
      let clientstate =
        Client.Mut.transitions clientstate trans
        |> Client.Mut.update_renderstate
      in

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;
      end_drawing ();

      (* Need this for the lwt scheduler *)
      let* () = Lwt_unix.sleep 0.0 in
      loop clientstate gamestate tc

let () =
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* clientstate, gamestate = Lwt.return (setup Game.Team.Blue) in
     let tc = tic2 () in
     loop clientstate gamestate tc)
