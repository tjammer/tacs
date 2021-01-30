open Sexplib0.Sexp_conv

type start = { starting : Game.Team.t; move_seed : int } [@@deriving sexp]

type t = Search | Found of Game.Team.t | Start of start | Move of Game.Input.t
[@@deriving sexp]

let string_of_t v = Sexplib0.Sexp.to_string_mach @@ sexp_of_t v

let parse msg =
  print_endline @@ "trying to parse : ." ^ msg ^ ".";
  match Sexplib.Sexp.of_string msg with
  | sexp -> (
      match t_of_sexp sexp with
      | msg -> Some msg
      | exception Sexplib.Conv.Of_sexp_error (_, _) -> None )
  | exception Sexplib.Conv.Of_sexp_error (_, _) -> None
