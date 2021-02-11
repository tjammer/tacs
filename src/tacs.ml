open ContainersLabels
open Lwt.Infix

let width = 1280

let height = 720

type bar = {
  width : int;
  border : int;
  height : int;
  outerc : Raylib.Color.t;
  innerc : Raylib.Color.t;
}

type buttonstate =
  | Idle of bar
  | Anim_hover of bar * bar * float
  | Anim_idle of bar * bar * float
  | Hover of bar

type mode = Local | Mp | Sp

type button = { bar : bar; x : int; y : int; state : buttonstate; mode : mode }

let button_xy { bar; x; y; state; mode = _ } =
  match state with
  | Idle _ -> (x, y)
  | Anim_hover (src, _, _) | Anim_idle (src, _, _) | Hover src ->
      (x - ((bar.width - src.width) / 2), y - ((bar.height - src.height) / 2))

let draw_bar b x y =
  (* draw outer border *)
  Raylib.draw_rectangle x y b.width b.height b.outerc;
  (* draw inner border *)
  Raylib.draw_rectangle (x + b.border) (y + b.border)
    (b.width - (2 * b.border))
    (b.height - (2 * b.border))
    Raylib.Color.raywhite;
  Raylib.draw_rectangle
    (x + (2 * b.border))
    (y + (2 * b.border))
    (b.width - (4 * b.border))
    (b.height - (4 * b.border))
    b.innerc;
  ()

let draw_bar_text b x y txt sz =
  let open Raylib in
  draw_bar b x y;
  let w = measure_text txt sz in
  let y = y + (b.height / 2) - (sz / 2) in
  let x = x + (b.width / 2) - (w / 2) in

  draw_text txt x y sz Color.raywhite;

  ()

let bar =
  {
    width = 450;
    border = 4;
    height = 150;
    outerc = Raylib.Color.gray;
    innerc = Raylib.Color.lightgray;
  }

let hover_bar =
  {
    width = 550;
    border = 4;
    height = 170;
    outerc = Raylib.Color.gray;
    innerc = Raylib.Color.skyblue;
  }

let animate_button a b t =
  {
    width =
      Client.Easings.quad_in_out ~t ~start:(Float.of_int a.width)
        ~final:(Float.of_int b.width) ~dur:1.0
      |> Int.of_float;
    border =
      Client.Easings.cubic_in ~t ~start:(Float.of_int a.border)
        ~final:(Float.of_int b.border) ~dur:1.0
      |> Int.of_float;
    height =
      Client.Easings.quad_in_out ~t ~start:(Float.of_int a.height)
        ~final:(Float.of_int b.height) ~dur:1.0
      |> Int.of_float;
    outerc = b.outerc;
    innerc = b.innerc;
  }

let layout_buttons bar lst x y w h =
  (* only vertically, take first bar as prototype *)
  let len = List.length lst in
  let xofs = (w - bar.width) / 2 in
  (* try to have same space above, below and between *)
  let vspace = (h - (len * bar.height)) / (len + 1) in
  List.mapi
    ~f:(fun i (state, mode) ->
      {
        bar;
        x = x + xofs;
        y = y + (((i + 1) * vspace) + (i * bar.height));
        state = Idle state;
        mode;
      })
    lst

let setup_window () =
  let open Raylib in
  set_config_flags [ ConfigFlag.VSync_hint ];
  init_window 1280 720 "tacs";
  set_exit_key Key.F12;
  layout_buttons bar
    [
      (hover_bar, Sp);
      ({ hover_bar with innerc = Color.orange }, Local);
      ({ hover_bar with innerc = Color.maroon }, Mp);
    ]
    80 0 (width / 2) height

let rec loop control buttons =
  let open Raylib in
  match (window_should_close (), control) with
  | true, `Cont | false, `Exit | true, `Exit ->
      close_window ();
      Lwt.return_unit
  | false, `Cont -> (
      let mx, my = (get_mouse_x (), get_mouse_y ()) in
      let buttons =
        List.map
          ~f:(fun but ->
            let x, y = button_xy but in
            let hover =
              mx > x
              && mx < x + but.bar.width
              && my > y
              && my < y + but.bar.height
            in
            match (but.state, hover) with
            | Idle dst, true ->
                { but with state = Anim_hover (but.bar, dst, 0.0) }
            | Idle _, false -> but
            | Anim_hover (src, dst, frac), true ->
                let frac = frac +. (1.0 /. 15.0) in
                if frac >=. 1.0 then { but with bar = dst; state = Hover src }
                else
                  {
                    but with
                    bar = animate_button src dst frac;
                    state = Anim_hover (src, dst, frac);
                  }
            | Anim_hover (src, dst, frac), false ->
                { but with state = Anim_idle (src, dst, frac) }
            | Anim_idle (src, dst, frac), false ->
                let frac = frac -. (1.0 /. 15.0) in
                if frac <=. 0.0 then { but with bar = src; state = Idle dst }
                else
                  {
                    but with
                    bar =
                      {
                        (animate_button src dst frac) with
                        innerc = Color.lightgray;
                      };
                    state = Anim_idle (src, dst, frac);
                  }
            | Anim_idle (src, dst, frac), true ->
                { but with state = Anim_hover (src, dst, frac) }
            | Hover _, true -> but
            | Hover src, false ->
                { but with state = Anim_idle (src, but.bar, 1.0) })
          buttons
      in

      match
        if is_mouse_button_pressed MouseButton.Left then
          List.fold_left
            ~f:(fun res bt ->
              match bt.state with
              | Idle _ | Anim_idle _ -> res
              | Hover _ | Anim_hover _ -> (
                  match bt.mode with
                  | Sp -> Some Solo.start
                  | Local -> Some Local.start
                  | Mp -> Some Multiplayer.connect ))
            ~init:None buttons
        else None
      with
      | Some f -> (
          f () >>= function
          | `Exit -> loop `Exit buttons
          | `Back -> loop `Cont buttons )
      | None ->
          begin_drawing ();
          clear_background Color.raywhite;

          List.iter
            ~f:(fun but ->
              let x, y = button_xy but in
              draw_bar_text but.bar x y
                ( match but.mode with
                | Local -> "Local"
                | Mp -> "Multiplayer"
                | Sp -> "Singleplayer" )
                50)
            buttons;

          let txt = "TACS" in
          let sz = 100 in
          let w = measure_text txt sz in
          let x = (width * 2 / 3) - (w / 6) in
          draw_text txt x 175 sz Color.gray;

          end_drawing ();
          if is_key_pressed Key.Escape then loop `Exit buttons
          else loop `Cont buttons )

let () = Lwt_main.run (setup_window () |> loop `Cont)
