open ContainersLabels
open Lwt.Infix

let rec game_over team ic oc msg clientstate gamestate (me, them) =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      let input =
        match Client.input clientstate Game.State.(gamestate.curr_team) with
        | Some input ->
            Lwt.async (fun () ->
                Lwt_io.write_line oc (Msg.string_of_t @@ Move input));
            Some input
        | None -> None
      in
      let me =
        match input with
        | Some (Select _) -> true
        | Some Deselect -> false
        | None -> me
      in

      let input, msg, control =
        match Lwt.state msg with
        | Return (Some msg) -> (
            print_endline msg;
            let new_msg = Lwt_io.read_line_opt ic in
            match Msg.parse msg with
            | Some (Move input) -> (Some input, new_msg, `Cont)
            | Some (Start { starting; move_seed }) ->
                let gamestate = Game.init starting move_seed in
                (None, new_msg, `Start gamestate)
            | _ -> (None, new_msg, `Cont) )
        | Return None | Fail _ -> (None, msg, `Abort)
        | Sleep -> (None, msg, `Cont)
      in

      let them =
        match input with
        | Some (Select _) -> true
        | Some Deselect -> false
        | None -> them
      in

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;

      let txt_size = measure_text "Rematch?" 80 in

      draw_text "Rematch?"
        ((get_screen_width () / 2) - (txt_size / 2))
        ((get_screen_height () / 2) + 40)
        80 Color.black;

      draw_rectangle
        ((get_screen_width () / 2) - 170)
        50 150 150 (fade Color.raywhite 0.75);

      draw_rectangle_lines
        ((get_screen_width () / 2) - 170)
        50 150 150 Color.gray;

      draw_rectangle
        ((get_screen_width () / 2) + 20)
        50 150 150 (fade Color.raywhite 0.75);

      draw_rectangle_lines
        ((get_screen_width () / 2) + 20)
        50 150 150 Color.gray;

      let pawn_blue, king_blue, _, king_red = Client.State.(clientstate.texs) in
      let scale =
        (150 |> Float.of_int) /. Float.of_int (Texture2D.width pawn_blue)
      in

      let tx = function Game.Team.Blue -> king_blue | Red -> king_red in

      if me then
        draw_texture_ex (tx clientstate.pov_team)
          (Vector2.create
             ((get_screen_width () / 2) - 170 |> Float.of_int)
             (Float.of_int 50))
          0.0 scale Color.white;

      if them then
        draw_texture_ex
          (tx (Game.Team.flip clientstate.pov_team))
          (Vector2.create
             ((get_screen_width () / 2) + 20 |> Float.of_int)
             (Float.of_int 50))
          0.0 scale Color.white;

      end_drawing ();

      let control = if is_key_pressed Key.Escape then `Abort else control in

      (* Need this for the lwt scheduler *)
      Lwt_unix.sleep 0.0 >>= fun () ->
      match control with
      | `Cont -> game_over team ic oc msg clientstate gamestate (me, them)
      | `Start gamestate -> loop ic oc msg clientstate gamestate
      | `Abort -> Lwt_io.close oc >>= fun () -> Lwt.return `Back )

and loop ic oc msg clientstate gamestate =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      let input, msg, control =
        match Lwt.state msg with
        | Return (Some msg) -> (
            print_endline msg;
            let new_msg = Lwt_io.read_line_opt ic in
            match Msg.parse msg with
            | Some (Move input) -> (Some input, new_msg, `Cont)
            | _ -> (None, new_msg, `Cont) )
        | Return None | Fail _ -> (None, msg, `Abort)
        | Sleep -> (None, msg, `Cont)
      in

      let input =
        if
          Game.(
            Team.equal
              State.(gamestate.curr_team)
              Client.State.(clientstate.pov_team))
        then
          match Client.input clientstate Game.State.(gamestate.curr_team) with
          | Some input ->
              Lwt.async (fun () ->
                  Lwt_io.write_line oc (Msg.string_of_t @@ Move input));
              Some input
          | None -> None
        else input
      in

      let trans = Game.transitions input gamestate in

      let gamestate = Game.Mut.apply trans gamestate in

      Client.Mut.apply clientstate trans;
      Client.Mut.update_renderstate clientstate;

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw clientstate gamestate;
      end_drawing ();

      let control = if is_key_pressed Key.Escape then `Abort else control in

      (* Need this for the lwt scheduler *)
      Lwt_unix.sleep 0.0 >>= fun () ->
      match (control, gamestate.state) with
      | `Cont, Game.Over team ->
          game_over team ic oc msg clientstate gamestate (false, false)
      | `Cont, _ -> loop ic oc msg clientstate gamestate
      | `Abort, _ -> Lwt_io.close oc >>= fun () -> Lwt.return `Back )

let rec wait_for_other ic oc msg clientstate =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
  | false -> (
      if is_key_pressed Key.Escape then
        Lwt_io.close oc >>= fun () -> Lwt.return `Back
      else
        match Lwt.state msg with
        | Return (Some msg) -> (
            print_endline msg;
            let new_msg = Lwt_io.read_line_opt ic in
            match Msg.parse msg with
            | Some (Start { starting; move_seed }) ->
                let gamestate = Game.init starting move_seed in
                loop ic oc new_msg clientstate gamestate
            | _ -> wait_for_other ic oc new_msg clientstate )
        | Return None -> Lwt.return `Back
        | Fail _ -> Lwt.return `Back
        | Sleep ->
            begin_drawing ();
            clear_background Color.raywhite;
            draw_text "waiting for other player..." 70
              ((get_screen_height () / 2) - 100)
              90 Color.black;
            end_drawing ();
            Lwt_unix.sleep 0.0 >>= fun () ->
            wait_for_other ic oc msg clientstate )

let rec wait_connect ic oc msg =
  let open Raylib in
  match window_should_close () with
  | true -> Lwt.return `Exit
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
      | Return None | Fail _ -> Lwt.return `Back
      | Sleep ->
          begin_drawing ();
          clear_background Color.raywhite;
          draw_text "Connecting..." 100
            ((get_screen_height () / 2) - 100)
            100 Color.black;
          end_drawing ();
          Lwt_unix.sleep 0.0 >>= fun () ->
          if is_key_pressed Key.Escape then Lwt.return `Back
          else wait_connect ic oc msg )

let connect addr () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let addr = addr in
  let addr =
    match ADDR_INET ((Unix.gethostbyname addr).h_addr_list.(0), 9000) with
    | addr -> addr
    | exception Not_found -> failwith "Could not resolve server address"
  in

  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let msg = Msg.string_of_t Search in

  let msg =
    connect sock addr >>= fun () ->
    Lwt_io.write_line oc msg >>= fun () -> Lwt_io.read_line_opt ic
  in
  wait_connect ic oc msg
