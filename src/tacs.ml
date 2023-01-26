open ContainersLabels
open Lwt.Infix

let width = 1280
let height = 720

type mode = Local | Mp | Sp

let setup_window () =
  let open Raylib in
  set_config_flags [ ConfigFlags.Vsync_hint ];
  init_window 1280 720 "tacs";
  set_exit_key Key.F12;
  set_window_icon (Lazy.force Client.king_bluei);
  Client.(
    Button.layout Bar.bar
      (List.map2 ~f:Pair.make Bar.bars [ Sp; Local; Mp ])
      80 0 (width / 2) height)

let rec loop control address buttons =
  let open Raylib in
  match (window_should_close (), control) with
  | true, `Cont | false, `Exit | true, `Exit ->
      close_window ();
      Lwt.return_unit
  | false, `Cont -> (
      let mx, my = (get_mouse_x (), get_mouse_y ()) in
      let buttons = List.map ~f:(Client.Button.update mx my) buttons in

      match
        if is_mouse_button_pressed MouseButton.Left then
          Client.Button.on_click buttons (function
            | Sp -> Some (Solo.select (width, height))
            | Local -> Some Local.start
            | Mp -> Some (Multiplayer.connect address))
        else None
      with
      | Some f -> (
          f () >>= function
          | `Exit -> loop `Exit address buttons
          | `Back -> loop `Cont address buttons)
      | None ->
          begin_drawing ();
          clear_background Color.raywhite;

          List.iter
            ~f:(fun but ->
              let x, y = Client.Button.xy but in
              Client.Bar.draw_text but.bar x y
                (match but.mode with
                | Local -> "Local"
                | Mp -> "Multiplayer"
                | Sp -> "Singleplayer")
                50)
            buttons;

          let txt = "TACS" in
          let sz = 100 in
          let w = measure_text txt sz in
          let x = (width * 2 / 3) - (w / 6) in
          draw_text txt x 175 sz Color.gray;

          end_drawing ();
          if is_key_pressed Key.Escape then loop `Exit address buttons
          else loop `Cont address buttons)

[@@@alert "-deprecated"]

let () =
  let addr =
    Cmdliner.(
      let default = None in
      let info =
        Arg.info [ "a"; "address" ] ~docv:"ADDR"
          ~doc:"Optional address of a server to connect to on multiplayer."
      in
      let term = Arg.value (Arg.opt (Arg.some Arg.string) default info) in
      match Term.eval (term, Term.info "tacs") with
      | `Error _ -> exit 1
      | `Version | `Help -> exit 0
      | `Ok (Some addr) -> addr
      | `Ok None -> "nils.cc")
  in

  Lwt_main.run (setup_window () |> loop `Cont addr)
