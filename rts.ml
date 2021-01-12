open Containers
module ImCoords = Helpers.ImmuHashtbl.Make (Hashtbl.Make (Tile.Coord))

let ( let* ) = Option.bind

let width = 1280

let height = 720

let layout = { Tile.size = { x = 60; y = 48 }; origin = { x = 0; y = 0 } }

let neighbors =
  [| (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) |]

let resource_amount_scale = 100.0

let collect_amount = 1

let num_tiles = 13

type resource_kind = First | Second

type collect = { from : Tile.Coord.t; kind : resource_kind }

type bank = { first : int; second : int }

(* TODO can this be made abstract *)
module Bank = struct
  let add t num = function
    | First -> { t with first = t.first + num }
    | Second -> { t with second = t.second + num }

  let get t = function First -> t.first | Second -> t.second
end

module Field = struct
  type t = { first : int; second : int }

  let random_scale_of_kind = function First -> 0.2 | Second -> 0.1

  let amount_scale_of_kind = function First -> 100.0 | Second -> 20.0

  let value_of t = function
    | First -> if t.first > 0 then Some t.first else None
    | Second -> if t.second > 0 then Some t.second else None

  let field_with t num = function
    | First -> { t with first = num }
    | Second -> { t with second = num }
end

module House = struct
  type t = {
    kind : resource_kind;
    price : resource_kind * int;
    timer : float;
    coord : Tile.Coord.t;
  }

  let collect_rate_of = function First -> 0.5 | Second -> 1.0

  let color_of_kind = function
    | First -> Raylib.Color.skyblue
    | Second -> Raylib.Color.orange

  let price_of_kind = function First -> (First, 200) | Second -> (First, 250)

  let of_coord ~kind coord =
    { kind; coord; price = price_of_kind kind; timer = collect_rate_of kind }
end

type event = Collect of collect | New_house of House.t

let noise2 x y ofs kind =
  let scale = Field.random_scale_of_kind kind in
  Noise.snoise2 ((x *. scale) +. ofs) ((y *. scale) +. ofs)

let apply_event (tbl, res, houses) ev =
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  match ev with
  | Collect { from; kind } ->
      let res =
        match
          let* field = Coordtbl.find_opt tbl from in
          let* value = Field.value_of field kind in
          let diff, before = (min collect_amount value, value) in
          Coordtbl.replace tbl from
            (Field.field_with field (before - diff) kind);
          Some (Bank.add res diff kind)
        with
        | Some res -> res
        | None -> res
      in
      (tbl, res, houses)
  | New_house house ->
      (* If we double click in the same frame then
       *  two houses could be bought at the same time, leave it for now *)
      ( match Coordtbl.find_opt tbl house.coord with
      | Some field ->
          Coordtbl.replace tbl house.coord (Field.field_with field 0 house.kind)
      | None -> () );
      let houses = house :: houses in
      let price_kind, price = house.price in
      let res = Bank.add res (-price) price_kind in
      (tbl, res, houses)

let sum_rate_of_neighbors (coord : Tile.Coord.t) tbl kind (sum, rate) (dx, dy) =
  let res =
    let* field = ImCoords.find_opt tbl { x = coord.x + dx; y = coord.y + dy } in
    let* value = Field.value_of field kind in
    let drate = Float.of_int collect_amount /. House.collect_rate_of kind in
    Some (sum + value, rate +. drate)
  in
  match res with Some res -> res | None -> (sum, rate)

let update_house house dt =
  let timer = House.(house.timer) -. dt in
  if timer <=. 0.0 then
    let crd = House.(house.coord) in
    let evs =
      Array.map
        (fun (dx, dy) ->
          Collect
            { from = { x = crd.x + dx; y = crd.y + dy }; kind = house.kind })
        neighbors
      |> Array.to_list
    in
    (evs, { house with timer = House.collect_rate_of house.kind })
  else ([], { house with timer })

let setup () =
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  let open Raylib in
  init_window width height "rts";
  set_target_fps 60;
  let texture = load_texture "assets/1bitpack/colored_packed.png" in

  Random.init 42;
  let rst = Random.get_state () in
  let ofs_first = Random.float 20.0 rst in
  let ofs_second = Random.float 200.0 rst in
  let tbl = Coordtbl.create 256 in

  let make_res_kind num kind =
    if num >. 0.0 then Int.of_float (num *. Field.amount_scale_of_kind kind)
    else 0
  in

  for y = 0 to num_tiles do
    for x = 0 to num_tiles * 4 / 3 do
      let first = noise2 (Float.of_int x) (Float.of_int y) ofs_first First in
      let second = noise2 (Float.of_int x) (Float.of_int y) ofs_second Second in
      Coordtbl.add tbl { x; y }
        {
          Field.first = make_res_kind first First;
          second = make_res_kind second Second;
        }
    done
  done;

  (texture, tbl)

let rec loop tex tbl res houses curr_kind buttons =
  let open Raylib in
  match window_should_close () with
  | true -> close_window ()
  | false ->
      let mpos = get_mouse_position () in
      let mx, my = Vector2.(x mpos, y mpos) in
      let coord = Tile.Coord.of_px mx my layout in

      let evs = [] in

      let evs, sum_rate, curr_kind =
        match curr_kind with
        | Some kind ->
            let sum, rate =
              Array.fold_left
                (fun accum neighbor ->
                  sum_rate_of_neighbors coord tbl kind accum neighbor)
                (0, 0.0) neighbors
            in

            let evs, curr_kind =
              let house = House.of_coord ~kind coord in
              let kind, price = house.price in
              if
                is_mouse_button_pressed MouseButton.Left
                && List.for_all
                     (fun other ->
                       not (Tile.Coord.equal coord House.(other.coord)))
                     houses
                && ImCoords.mem tbl coord
                && Bank.get res kind >= price
              then (New_house house :: evs, None)
              else if is_mouse_button_pressed MouseButton.Right then (evs, None)
              else (evs, curr_kind)
            in
            (evs, Some (sum, rate), curr_kind)
        | None ->
            let curr_kind =
              List.fold_while
                (fun _ (rect, kind) ->
                  let price_kind, price = House.price_of_kind kind in
                  if
                    Bank.get res price_kind >= price
                    && is_mouse_button_pressed MouseButton.Left
                    && check_collision_point_rec mpos rect
                  then (Some kind, `Stop)
                  else (None, `Continue))
                None buttons
            in
            (evs, None, curr_kind)
      in

      let tiles =
        Array.filter_map
          (fun (dx, dy) ->
            let tile = { Tile.x = coord.x + dx; y = coord.y + dy } in
            if ImCoords.mem tbl tile then Some tile else None)
          neighbors
      in

      let new_evs, houses =
        List.fold_map
          (fun evs house ->
            let new_evs, house = update_house house (1.0 /. 60.0) in
            (new_evs @ evs, house))
          [] houses
      in
      let evs = new_evs @ evs in

      let tbl, res, houses =
        List.fold_left apply_event (tbl, res, houses) evs
      in

      begin_drawing ();
      clear_background Color.raywhite;

      let x, y = Tile.Coord.to_px coord layout in
      if Option.is_some curr_kind then (
        Array.iter
          (fun tl ->
            let x, y = Tile.Coord.to_px tl layout in
            draw_rectangle x y layout.size.x layout.size.y Color.lightgray)
          tiles;

        draw_rectangle x y layout.size.x layout.size.y
          (fade (House.color_of_kind (Option.get_exn curr_kind)) 0.5) );

      List.iter
        (fun house ->
          let x, y = Tile.Coord.to_px House.(house.coord) layout in
          draw_rectangle x y layout.size.x layout.size.y
            (House.color_of_kind house.kind))
        houses;

      (* draw grid *)
      for x = 0 to (num_tiles + 1) * 4 / 3 do
        draw_line_ex
          (Vector2.create (Float.of_int (x * layout.size.x)) 0.0)
          (Vector2.create
             (Float.of_int (x * layout.size.x))
             (Float.of_int ((num_tiles + 1) * layout.size.y)))
          2.0 Color.gray
      done;
      for y = 0 to num_tiles + 1 do
        draw_line_ex
          (Vector2.create 0.0 (Float.of_int (y * layout.size.y)))
          (Vector2.create
             (Float.of_int ((num_tiles + 1) * 4 / 3 * layout.size.x))
             (Float.of_int (y * layout.size.y)))
          2.0 Color.gray
      done;

      ImCoords.iter
        (fun coord field ->
          let x, y = Tile.Coord.to_px coord layout in
          draw_text (Printf.sprintf "%i" Field.(field.first)) x y 20 Color.blue;
          draw_text
            (Printf.sprintf "%i" Field.(field.second))
            x (y + 25) 20 Color.maroon)
        tbl;

      ( match sum_rate with
      | Some (sum, rate) ->
          draw_text
            (Printf.sprintf "%.1f" rate)
            (x + (layout.size.x / 5))
            y 30 Color.black;
          draw_text (Printf.sprintf "%i" sum)
            (x + (layout.size.x / 5))
            (y + 30) 30 Color.black
      | None -> () );

      draw_text
        (Printf.sprintf "1st: %i" res.first)
        (width - 150) 10 30 Color.black;

      draw_text
        (Printf.sprintf "2nd: %i" res.second)
        (width - 150) (10 + 40) 30 Color.black;

      List.iter
        (fun (rect, kind) ->
          let price_kind, price = House.price_of_kind kind in
          let color =
            if Bank.get res price_kind >= price then House.color_of_kind kind
            else fade (House.color_of_kind kind) 0.3
          in
          draw_rectangle_rec rect color)
        buttons;

      end_drawing ();
      loop tex tbl res houses curr_kind buttons

let rect_of_ints x y width height =
  let open Float in
  Raylib.Rectangle.create (of_int x) (of_int y) (of_int width) (of_int height)

let buttons =
  [
    (rect_of_ints (width - 150) (height - 250) 100 80, First);
    (rect_of_ints (width - 150) (height - 150) 100 80, Second);
  ]

let () =
  let tex, map = setup () in
  loop tex map { first = 200; second = 0 } [] None buttons
