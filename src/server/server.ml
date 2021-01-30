open Lwt

let ( let* ) = Lwt.bind

module Player = struct
  type t = {
    team : Game.Team.t;
    conn : Lwt_unix.file_descr;
    ic : Lwt_io.input_channel;
    oc : Lwt_io.output_channel;
  }
end

module Board = struct
  (* a board with one waiting player *)
  type t = Game.State.t * Player.t * unit Lwt.t
end

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (Unix.inet_addr_any, 9000) >>= fun () ->
  listen sock 10;
  return sock

let board_wait = ref []

(* TODO hashtbl gamestate which we mutate *)

(* match Sexplib.Sexp.parse msg with
 * | Done (sexp, _) ->
 *     let msg = Msg.t_of_sexp sexp in
 *     Some msg
 * | Cont _ ->
 *     print_endline @@ "could not parse sexp " ^ msg;
 *     None *)

let start_game oc1 oc2 move_seed =
  let msg = Msg.string_of_t @@ Start { starting = Game.Team.Blue; move_seed } in
  let m1 = Lwt_io.write_line oc1 msg in
  let m2 = Lwt_io.write_line oc2 msg in
  Lwt.join [ m1; m2 ]

let rec wait player =
  let* msg = Lwt_io.read_line_opt Player.(player.ic) in
  match msg with
  | Some _ ->
      (* we ignore input, they should be waiting *)
      wait player
  | None ->
      (* connection dropped *)
      (* TODO mutex *)
      print_endline "waiting player dropped";
      board_wait := [];
      return_unit

let msg_with_player p =
  let* msg = Lwt_io.read_line_opt Player.(p.ic) in
  Lwt.return (msg, p)

let rec play gs p1 p2 =
  let* msg, player = [ p1; p2 ] |> List.map msg_with_player |> Lwt.pick in
  match Option.bind msg Msg.parse with
  | Some (Msg.Move input) ->
      if not (Game.Team.equal player.team Game.State.(gs.curr_team)) then (
        print_endline
          "unexpected team. we should drop,b/c the player cliend cannot recover";
        play gs p1 p2 )
      else
        let () = print_endline "in else" in
        let trans = Game.transitions (Some input) gs in
        let gs = Game.Mut.apply trans gs in
        (* write to other player *)
        let () = print_endline "before if" in
        let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
        let () = print_endline "after if " in
        let* () = Lwt_io.write_line oc (Option.get msg) in
        let () = print_endline "after write " in
        play gs p1 p2
  | Some (Start _) | Some (Found _) | Some Search ->
      print_endline "unexpected message in handle message. drop";
      return_unit
  | None ->
      print_endline "could not parse or disconnect. either way we drop both lol";
      (* let ic, oc =
       *   if Game.Team.equal player.team p1.team then (p1.ic, p1.oc)
       *   else (p2.ic, p2.oc)
       * in
       * let ic = Lwt_io.close ic in
       * let oc = Lwt_io.close oc in
       * let* () = Lwt.join [ ic; oc ] in *)
      return_unit

let initial_connection conn ~ic ~oc =
  let* msg = Lwt_io.read_line_opt ic in
  match Option.bind msg (fun msg -> Msg.parse msg) with
  | Some Msg.Search -> (
      (* TODO mutex *)
      match !board_wait with
      | [] ->
          (* first player *)
          let team = Game.Team.Blue in
          let gamestate = Game.init team 0 in

          let ret = Msg.string_of_t @@ Found team in
          let* () = Lwt_io.write_line oc ret in
          let player = { Player.team; conn; ic; oc } in
          let wait_p = wait player in
          board_wait := [ (gamestate, player, wait_p) ];
          return_unit
      | (gamestate, player1, wait) :: _ ->
          let team = Game.Team.flip gamestate.curr_team in
          (* TODO mutex *)
          board_wait := [];
          (* cancel first player promise to handle both in play () *)
          Lwt.cancel wait;

          let ret = Msg.string_of_t @@ Found team in
          let* () = Lwt_io.write_line oc ret in
          let player2 = { Player.team; conn; ic; oc } in
          let* () = start_game player1.oc player2.oc 0 in
          play gamestate player1 player2 )
  | Some (Start _) | Some (Move _) | Some (Found _) ->
      print_endline "unexpected message in 1st conn";
      let* () = Lwt_io.close oc in
      let* () = Lwt_io.close ic in
      return_unit
  | None ->
      print_endline "drop";
      let* () = Lwt_io.close oc in
      let* () = Lwt_io.close ic in
      return_unit

let handle_connection (sock, _) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  print_endline "new connection";
  initial_connection sock ~ic ~oc

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= handle_connection >>= serve in
  serve

let () =
  Lwt_main.run
    ( create_socket () >>= fun sock ->
      let serve = create_server sock in
      serve () )
