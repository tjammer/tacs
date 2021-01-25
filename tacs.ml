open Containers

let ( let* ) = Lwt.bind

let width = 1280

let height = 720

let tic2 () =
  let* () = Lwt_unix.sleep 1.0 in
  Lwt_io.printl "tix%!"

let setup () =
  let module Coordtbl = Hashtbl.Make (Tile.Coord) in
  let module Movetbl = Hashtbl.Make (Moves.Movekey) in
  let open Raylib in
  init_window 1280 720 "tacs";
  set_target_fps 60;
  let pawn_blue = load_texture "assets/blue_pawn.png" in
  let king_blue = load_texture "assets/blue_king.png" in
  let pawn_red = load_texture "assets/red_pawn.png" in
  let king_red = load_texture "assets/red_king.png" in
  let texs = (pawn_blue, king_blue, pawn_red, king_red) in

  let tbl, moves = Game.(restart Team.Blue) in

  let ent_anims = Coordtbl.create 10 in
  let move_anims = Movetbl.create 5 in

  (texs, ent_anims, move_anims, tbl, moves)

let rec loop texs ent_anims move_anims tbl (state, team) moves tc =
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

      let input = Client.input team in

      let trans = Game.transitions input tbl state team moves in

      (* NOTE the hashtbl is mutated here *)
      let state, team, moves = Game.Mut.apply trans tbl state team moves in

      let () = Client.Mut.transitions ent_anims move_anims trans in
      let () = Client.Mut.update_renderstate ent_anims move_anims in

      begin_drawing ();
      clear_background Color.raywhite;
      Client.draw texs ent_anims move_anims tbl state team moves;

      end_drawing ();

      (* Need this for the lwt scheduler *)
      let* () = Lwt_unix.sleep 0.0 in
      loop texs ent_anims move_anims tbl (state, team) moves tc

let () =
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* texs, ent_anims, move_anims, tbl, moves = Lwt.return (setup ()) in
     let tc = tic2 () in
     loop texs ent_anims move_anims tbl (Choose_move, Blue) moves tc)
