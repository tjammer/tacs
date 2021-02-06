open Lwt.Infix

let width = 1280

let height = 720

let setup_window () =
  let open Raylib in
  set_config_flags [ ConfigFlag.VSync_hint ];
  init_window 1280 720 "tacs";
  set_exit_key Key.F12;
  ()

let rec loop control =
  let open Raylib in
  match (window_should_close (), control) with
  | true, `Cont | false, `Exit | true, `Exit ->
      close_window ();
      Lwt.return_unit
  | false, `Cont ->
      if is_key_pressed Key.Left then
        Multiplayer.connect () >>= function
        | `Exit -> loop `Exit
        | `Back -> loop `Cont
      else if is_key_pressed Key.Right then
        Local_coop.start () >>= function
        | `Exit -> loop `Exit
        | `Back -> loop `Cont
      else (
        begin_drawing ();
        clear_background Color.raywhite;
        draw_text "hello" 200 200 100 Color.white;
        end_drawing ();
        loop `Cont )

let () =
  Lwt_main.run
    ( setup_window ();
      loop `Cont )
