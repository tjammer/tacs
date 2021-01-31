let ( let* ) = Lwt.bind

let width = 1280

let height = 720

let tic2 () =
  let* () = Lwt_unix.sleep 1.0 in
  Lwt_io.printl "tix%!"

let setup_window () =
  let open Raylib in
  init_window 1280 720 "tacs";
  set_target_fps 60;
  ()

let rec loop ic oc msg clientstate gamestate =
  let open Raylib in
  match window_should_close () with
  | true ->
      close_window ();
      Lwt.return_unit
  | false ->
      let input, msg =
        if
          Game.State.(gamestate.curr_team) = Client.State.(clientstate.pov_team)
        then
          match Client.input clientstate Game.State.(gamestate.curr_team) with
          | Some input ->
              Lwt.async (fun () ->
                  Lwt_io.write_line oc (Msg.string_of_t @@ Move input));
              (Some input, msg)
          | None -> (None, msg)
        else
          match Lwt.state msg with
          | Return (Some msg) -> (
              print_endline msg;
              let new_msg = Lwt_io.read_line_opt ic in
              match Msg.parse msg with
              | Some (Move input) -> (Some input, new_msg)
              | _ -> (None, new_msg) )
          | Return None | Fail _ ->
              print_endline "cancel?";
              close_window ();
              (None, msg)
          | Sleep -> (None, msg)
      in

      let trans = Game.transitions input gamestate in

      let gamestate = Game.Mut.apply trans gamestate in

      Client.Mut.apply clientstate trans;
      Client.Mut.update_renderstate clientstate;

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;
      end_drawing ();

      (* Need this for the lwt scheduler *)
      let* () = Lwt_unix.sleep 0.0 in
      loop ic oc msg clientstate gamestate

let rec wait_for_other ic oc msg clientstate =
  let open Raylib in
  match window_should_close () with
  | true ->
      close_window ();
      Lwt.return_unit
  | false -> (
      match Lwt.state msg with
      | Return (Some msg) -> (
          print_endline msg;
          let new_msg = Lwt_io.read_line_opt ic in
          match Msg.parse msg with
          | Some (Start { starting; move_seed }) ->
              let gamestate = Game.init starting move_seed in
              loop ic oc new_msg clientstate gamestate
          | _ -> wait_for_other ic oc new_msg clientstate )
      | Return None -> Lwt.return_unit
      | Fail _ -> Lwt.return_unit
      | Sleep ->
          begin_drawing ();
          clear_background Color.raywhite;
          draw_text "waiting for other player..." 70
            ((get_screen_height () / 2) - 100)
            90 Color.black;
          end_drawing ();
          let* () = Lwt_unix.sleep 0.0 in
          wait_for_other ic oc msg clientstate )

let rec wait_connect ic oc msg =
  let open Raylib in
  match window_should_close () with
  | true ->
      close_window ();
      Lwt.return_unit
  | false -> (
      match Lwt.state msg with
      | Return (Some msg) -> (
          print_endline msg;
          let new_msg = Lwt_io.read_line_opt ic in
          match Msg.parse msg with
          | Some (Found team) ->
              let clientstate = Client.init_state team in
              wait_for_other ic oc new_msg clientstate
          | _ -> wait_connect ic oc new_msg )
      | Return None -> Lwt.return_unit
      | Fail _ -> Lwt.return_unit
      | Sleep ->
          begin_drawing ();
          clear_background Color.raywhite;
          draw_text "Connecting..." 100
            ((get_screen_height () / 2) - 100)
            100 Color.black;
          end_drawing ();
          let* () = Lwt_unix.sleep 0.0 in
          wait_connect ic oc msg )

let connect () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = ADDR_INET (Unix.inet_addr_of_string "192.168.0.101", 9000) in
  (* TODO this should also loop  *)
  let* () = connect sock addr in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let msg = Msg.string_of_t Search in
  let* () = Lwt_io.write_line oc msg in
  setup_window ();
  let msg = Lwt_io.read_line_opt ic in
  wait_connect ic oc msg

let rec get_pong ic oc =
  let* msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some "pong" -> Lwt.return_unit
  | Some other ->
      print_endline @@ "other: " ^ other;
      let* () = Lwt_io.write_line oc "ping" in
      get_pong ic oc
  | None -> failwith "connection failed"

let () =
  (* Lwt_main.run
   *   (let ( let* ) = Lwt.bind in
   *    setup_window ();
   *    let* clientstate = Lwt.return (Client.init_state Game.Team.Blue) in
   *    let gamestate = Game.restart Game.Team.Blue in
   *
   *    let open Lwt_unix in
   *    let sock = socket PF_INET SOCK_STREAM 0 in
   *    let addr = ADDR_INET (Unix.inet_addr_of_string "192.168.0.101", 9000) in
   *    let* () = connect sock addr in
   *    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
   *    (\* let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in *\)
   *    let* () = Lwt_io.write_line oc "hello" in
   *    (\* let* () = get_pong ic oc in *\)
   *    loop clientstate gamestate) *)
  Lwt_main.run (connect ())
