open ContainersLabels

module ImmuHashtbl = struct
  module type S = sig
    type key
    type 'a t

    val find_opt : 'a t -> key -> 'a option
    val find_exn : 'a t -> key -> 'a
    val mem : 'a t -> key -> bool
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

  module Make (H : Hashtbl.S) : S with type key = H.key and type 'a t = 'a H.t =
  struct
    include H

    let find_exn = find
  end
end

module Easings = struct
  type t = Linear | Cubic | Quad

  let linear ~t ~start ~final ~dur = ((final -. start) *. t /. dur) +. start

  let cubic_in ~t ~start ~final ~dur =
    let t = t /. dur in
    ((final -. start) *. t *. t *. t) +. start

  let quad_in_out ~t ~start ~final ~dur =
    let final = final -. start in
    let t = t /. dur *. 2.0 in
    if t <. 1.0 then (final /. 2.0 *. (t *. t)) +. start
    else (-.final /. 2.0 *. (((t -. 1.0) *. (t -. 3.0)) -. 1.0)) +. start
end

module Bar = struct
  type t = {
    width : int;
    border : int;
    height : int;
    outerc : Raylib.Color.t;
    innerc : Raylib.Color.t;
  }

  let draw b x y =
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

  let draw_text b x y txt sz =
    let open Raylib in
    draw b x y;
    let w = measure_text txt sz in
    let y = y + (b.height / 2) - (sz / 2) in
    let x = x + (b.width / 2) - (w / 2) in

    draw_text txt x y sz Color.raywhite;

    ()

  let animate a b t =
    {
      width =
        Easings.quad_in_out ~t ~start:(Float.of_int a.width)
          ~final:(Float.of_int b.width) ~dur:1.0
        |> Int.of_float;
      border =
        Easings.cubic_in ~t ~start:(Float.of_int a.border)
          ~final:(Float.of_int b.border) ~dur:1.0
        |> Int.of_float;
      height =
        Easings.quad_in_out ~t ~start:(Float.of_int a.height)
          ~final:(Float.of_int b.height) ~dur:1.0
        |> Int.of_float;
      outerc = b.outerc;
      innerc = b.innerc;
    }

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

  let bars =
    [
      hover_bar;
      { hover_bar with innerc = Raylib.Color.orange };
      { hover_bar with innerc = Raylib.Color.maroon };
    ]
end

module Button = struct
  type state =
    | Idle of Bar.t
    | Anim_hover of Bar.t * Bar.t * float
    | Anim_idle of Bar.t * Bar.t * float
    | Hover of Bar.t

  type 'a t = { bar : Bar.t; x : int; y : int; state : state; mode : 'a }

  let xy { bar; x; y; state; mode = _ } =
    match state with
    | Idle _ -> (x, y)
    | Anim_hover (src, _, _) | Anim_idle (src, _, _) | Hover src ->
        (x - ((bar.width - src.width) / 2), y - ((bar.height - src.height) / 2))

  let layout bar lst x y w h =
    (* only vertically, take first bar as prototype *)
    let len = List.length lst in
    let xofs = (w - Bar.(bar.width)) / 2 in
    (* try to have same space above, below and between *)
    let vspace = (h - (len * Bar.(bar.height))) / (len + 1) in
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

  let update mx my but =
    let x, y = xy but in
    let hover =
      mx > x && mx < x + but.bar.width && my > y && my < y + but.bar.height
    in
    match (but.state, hover) with
    | Idle dst, true -> { but with state = Anim_hover (but.bar, dst, 0.0) }
    | Idle _, false -> but
    | Anim_hover (src, dst, frac), true ->
        let frac = frac +. (1.0 /. 15.0) in
        if frac >=. 1.0 then { but with bar = dst; state = Hover src }
        else
          {
            but with
            bar = Bar.animate src dst frac;
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
                (Bar.animate src dst frac) with
                innerc = Raylib.Color.lightgray;
              };
            state = Anim_idle (src, dst, frac);
          }
    | Anim_idle (src, dst, frac), true ->
        { but with state = Anim_hover (src, dst, frac) }
    | Hover _, true -> but
    | Hover src, false -> { but with state = Anim_idle (src, but.bar, 1.0) }

  let on_click buttons fn =
    List.fold_left
      ~f:(fun res bt ->
        match bt.state with
        | Idle _ | Anim_idle _ -> res
        | Hover _ | Anim_hover _ -> fn bt.mode)
      ~init:None buttons
end
