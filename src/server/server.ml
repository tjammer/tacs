open ContainersLabels
open Lwt
open Lwt.Infix

module Player = struct
  type t = {
    team : Game.Team.t;
    conn : Lwt_unix.file_descr;
    ic : Lwt_io.input_channel;
    oc : Lwt_io.output_channel;
  }

  let equal a b = Game.Team.equal a.team b.team
end

module Board = struct
  (* a board with one waiting player *)
  type t = Game.State.t * Player.t * unit Lwt.t
end

let board_wait = ref []

let board_mutex = Lwt_mutex.create ()

let () = Random.self_init ()

let rst = Random.get_state ()

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (Unix.inet_addr_any, 9000) >>= fun () ->
  listen sock 10;
  return sock

let start_game oc1 oc2 starting move_seed =
  let msg = Msg.string_of_t @@ Start { starting; move_seed } in
  let m1 = Lwt_io.write_line oc1 msg in
  let m2 = Lwt_io.write_line oc2 msg in
  Lwt.join [ m1; m2 ]

let rec wait player =
  Lwt_io.read_line_opt Player.(player.ic) >>= function
  | Some _ ->
      (* we ignore input, they should be waiting *)
      wait player
  | None -> (
      (* connection dropped *)
      Lwt_mutex.lock board_mutex >>= fun () ->
      match !board_wait with
      | (player1, _) :: _ when Game.Team.equal player.team Player.(player1.team)
        ->
          print_endline "waiting player dropped";
          board_wait := [];
          Lwt_mutex.unlock board_mutex;
          return_unit
      | _ ->
          Lwt_mutex.unlock board_mutex;
          return_unit )

let msg_with_player p =
  Lwt_io.read_line_opt Player.(p.ic) >>= fun msg -> Lwt.return (msg, p)

let max_seed = (1 lsl 30) - 1

let rec game_over losing_team p1 p2 rematch =
  [ p1; p2 ] |> List.map ~f:msg_with_player |> Lwt.pick >>= fun (msg, player) ->
  match
    Option.bind msg @@ fun msg ->
    print_endline @@ msg ^ "ccc";
    Option.bind (Msg.parse msg) @@ fun parsed -> Some (msg, parsed)
  with
  | Some (msg, Msg.Move (Select _)) ->
      let rematch =
        if not (List.mem ~eq:Player.equal player rematch) then player :: rematch
        else rematch
      in

      if List.length rematch = 2 then
        let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
        Lwt_io.write_line oc msg >>= fun () ->
        let seed = Random.int max_seed rst in
        let gamestate = Game.init losing_team seed in

        start_game p1.oc p2.oc losing_team seed >>= fun () ->
        play gamestate p1 p2
      else
        let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
        Lwt_io.write_line oc msg >>= fun () ->
        game_over losing_team p1 p2 rematch
  | Some (msg, Msg.Move Deselect) ->
      let rematch =
        List.filter_map
          ~f:(fun (p : Player.t) ->
            if Game.Team.equal player.team p.team then None else Some p)
          rematch
      in
      let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
      Lwt_io.write_line oc msg >>= fun () -> game_over losing_team p1 p2 rematch
  | Some (_, Start _) | Some (_, Found _) | Some (_, Search) ->
      print_endline "unexpected message in handle message. drop";
      return_unit
  | None ->
      print_endline
        "over could not parse or disconnect. either way we drop both lol";
      let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
      Lwt_io.close oc >>= fun () -> return_unit

and play gs p1 p2 =
  [ p1; p2 ] |> List.map ~f:msg_with_player |> Lwt.pick >>= fun (msg, player) ->
  match Option.bind msg Msg.parse with
  | Some (Msg.Move input) -> (
      if not (Game.Team.equal player.team Game.State.(gs.curr_team)) then (
        print_endline
          "unexpected team. we should drop,b/c the player cliend cannot recover";

        play gs p1 p2 )
      else
        let trans = Game.transitions (Some input) gs in
        let gs = Game.Mut.apply trans gs in
        (* write to other player *)
        let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
        Lwt_io.write_line oc (Option.get_exn msg) >>= fun () ->
        match gs.state with
        | Game.Over team -> game_over team p1 p2 []
        | _ -> play gs p1 p2 )
  | Some (Start _) | Some (Found _) | Some Search ->
      print_endline "unexpected message in handle message. drop";
      return_unit
  | None ->
      print_endline "could not parse or disconnect. either way we drop both lol";
      let oc = if Game.Team.equal player.team p1.team then p2.oc else p1.oc in
      Lwt_io.close oc >>= fun () -> return_unit

let initial_connection conn ~ic ~oc =
  Lwt.try_bind
    (fun () -> Lwt_io.read_line_opt ic)
    (fun msg ->
      match Option.bind msg (fun msg -> Msg.parse msg) with
      | Some Msg.Search -> (
          Lwt_mutex.lock board_mutex >>= fun () ->
          match !board_wait with
          | [] ->
              (* first player *)
              let team = Random.pick_list [ Game.Team.Blue; Red ] rst in
              let ret = Msg.string_of_t @@ Found team in
              Lwt_io.write_line oc ret >>= fun () ->
              let player = { Player.team; conn; ic; oc } in
              let wait_p = wait player in

              board_wait := [ (player, wait_p) ];
              Lwt_mutex.unlock board_mutex;

              return_unit
          | (player1, wait) :: _ ->
              board_wait := [];
              (* cancel first player promise to handle both in play () *)
              Lwt.cancel wait;
              Lwt_mutex.unlock board_mutex;

              let team = Game.Team.flip player1.team in
              let ret = Msg.string_of_t @@ Found team in
              Lwt_io.write_line oc ret >>= fun () ->
              let player2 = { Player.team; conn; ic; oc } in

              let seed = Random.int max_seed rst in
              let start_team = Game.Team.Blue in
              let gamestate = Game.init start_team seed in

              start_game player1.oc player2.oc start_team seed >>= fun () ->
              play gamestate player1 player2 )
      | Some (Start _) | Some (Move _) | Some (Found _) ->
          print_endline "unexpected message in 1st conn";
          Lwt.try_bind
            (fun () -> Lwt_io.close oc >>= fun () -> Lwt_io.close ic)
            (fun () -> return_unit)
            (fun _ -> return_unit)
      | None ->
          print_endline "drop";
          Lwt.try_bind
            (fun () -> Lwt_io.close oc >>= fun () -> Lwt_io.close ic)
            (fun () -> return_unit)
            (fun _ -> return_unit))
    (fun _ -> return_unit)

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
