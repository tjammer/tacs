open ContainersLabels
open Game

type move_seq = {
  move : Game.Moves.Movekey.t;
  ent : Game.Tile.Coord.t;
  coord : Game.Tile.Coord.t;
}

let inp_of_move = function
  | Game.Moves.Movekey.Blue_left | Red_left -> Game.Input.Left
  | Blue_right | Red_right -> Right
  | Middle -> assert false

(* TODO generate sequence of all moves *)
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
  | Over _ -> gs
  | Choose_move ->
      let apply = Fun.flip Game.Mut.apply in
      let gs =
        Game.transitions (Some (Select (`Move (inp_of_move seq.move)))) gs
        |> apply gs
      in
      let gs = Game.transitions (Some (Select (`Ent seq.ent))) gs |> apply gs in
      Game.transitions (Some (Select (`Ent seq.coord))) gs |> apply gs

type move_tree = { gs : Game.State.t; children : (move_seq * move_tree) list }

let rec fill_tree gs depth =
  if depth = 0 then { gs; children = [] }
  else
    let moves = find_possible_moves gs in
    {
      gs;
      children =
        List.map
          ~f:(fun seq ->
            let gs = advance gs seq in
            (seq, fill_tree gs (depth - 1)))
          moves;
    }

let best_move depth gs =
  let tree = fill_tree gs depth in

  let rec score_tree { gs; children } ct sm =
    match children with
    | [] -> (None, sm)
    | lst ->
        let op = if ct mod 2 = 0 then ( + ) else ( - ) in
        let seq, score =
          List.map
            ~f:(fun (({ move; ent; coord } as seq), tree) ->
              let score =
                outcome_of_selected gs
                  (List.Assoc.get_exn ~eq:Moves.Movekey.equal move gs.moves, ent)
                  coord
                |> Option.get_exn |> score_move
              in
              let _, ch_score = score_tree tree (ct + 1) score in
              (seq, ch_score))
            lst
          |> List.sort ~cmp:(fun (_, a) (_, b) -> compare b a)
          |> List.hd
        in
        (Some seq, op sm score)
  in
  let seq, _ = score_tree tree 0 0 in
  Option.get_exn seq
