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

  let clientstate = Client.init_state team in
  let gamestate = Game.(restart team) in

  (clientstate, gamestate)

let rec loop clientstate gamestate =
  let open Raylib in
  match window_should_close () with
  | true ->
      close_window ();
      Lwt.return_unit
  | false ->
      (* let tc =
       *   match Lwt.state tc with
       *   | Sleep -> tc
       *   | Return _ -> tic2 ()
       *   | Fail _ -> tic2 ()
       * in *)
      let input = Client.input clientstate Game.State.(gamestate.curr_team) in
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
      loop clientstate gamestate

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
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* clientstate, gamestate = Lwt.return (setup Game.Team.Blue) in
     (* let tc = tic2 () in *)
     let open Lwt_unix in
     let sock = socket PF_INET SOCK_STREAM 0 in
     let addr = ADDR_INET (Unix.inet_addr_of_string "192.168.0.101", 9000) in
     let* () = connect sock addr in
     let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
     let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
     let* () = Lwt_io.write_line oc "hello" in
     let* () = get_pong ic oc in
     loop clientstate gamestate)
