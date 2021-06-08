open ContainersLabels
open Game

let () = Random.self_init ()

let rst = Random.get_state ()

type move_seq = {
  move : Game.Moves.Movekey.t;
  ent : Game.Tile.Coord.t;
  coord : Game.Tile.Coord.t;
}

let def_seq =
  {
    move = Game.Moves.Movekey.Middle;
    ent = { Game.Tile.x = 0; y = 0 };
    coord = { Game.Tile.x = 0; y = 0 };
  }

let inp_of_move = function
  | Game.Moves.Movekey.Blue_left | Red_left -> Game.Input.Left
  | Blue_right | Red_right -> Right
  | Middle -> assert false

let moves team =
  match team with
  | Team.Blue -> [ Moves.Movekey.Blue_left; Blue_right ]
  | Red -> [ Red_left; Red_right ]

let ents ents team =
  List.filter_map
    ~f:(fun (ent, (_, tm)) -> if Team.equal team tm then Some ent else None)
    ents

let find_possible_moves gs =
  let eq = Moves.Movekey.equal in
  moves State.(gs.curr_team)
  |> List.product ~f:Pair.make (ents gs.ents gs.curr_team)
  |> List.map ~f:(fun (ent, move) ->
         List.map
           ~f:(fun mv ->
             { move; ent; coord = Tile.{ x = ent.x + mv.x; y = ent.y + mv.y } })
           (List.Assoc.get_exn ~eq move gs.moves))
  |> List.flatten
  |> List.filter ~f:(fun { move; ent; coord } ->
         Option.is_some
           (outcome_of_selected gs
              (List.Assoc.get_exn ~eq move gs.moves, ent)
              coord))

let score_move = function
  | Game.Take -> 1
  | Move -> 0
  | Win_move | Win_take -> 2

let advance gs seq =
  match Game.State.(gs.state) with
  | Choose_ent _ | Move _ -> assert false
  | Over _ -> None
  | Choose_move ->
      let apply = Fun.flip Game.Mut.apply in
      let gs =
        Game.transitions (Some (Select (`Move (inp_of_move seq.move)))) gs
        |> apply gs
      in
      let gs = Game.transitions (Some (Select (`Ent seq.ent))) gs |> apply gs in
      Game.transitions (Some (Select (`Ent seq.coord))) gs
      |> apply gs |> Option.pure

let rec best_move depth gs =
  if depth = 0 then (def_seq, 0)
  else
    let moves = find_possible_moves gs in
    let seq, score =
      List.map
        ~f:(fun ({ move; ent; coord } as seq) ->
          let score =
            outcome_of_selected gs
              (List.Assoc.get_exn ~eq:Moves.Movekey.equal move gs.moves, ent)
              coord
            |> Option.get_exn_or "wrong outcame"
            |> score_move
          in
          match score with
          | 2 -> (seq, score)
          | _ ->
              let _, ch_score =
                best_move (depth - 1)
                  (advance gs seq |> Option.get_exn_or "no best move")
              in
              (seq, score - ch_score))
        moves
      |> List.sort ~cmp:(fun (_, a) (_, b) -> compare b a)
      |> List.group_succ ~eq:(fun (_, a) (_, b) -> a = b)
      |> List.hd
      |> (Fun.flip Random.pick_list) rst
    in
    (seq, score)
