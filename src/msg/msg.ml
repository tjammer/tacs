module Sexp = Csexp.Make (Sexplib0.Sexp)
open Sexplib0.Sexp_conv

type start = { starting : Game.Team.t; move_seed : int } [@@deriving sexp]

type t = Search | Found of Game.Team.t | Start of start | Move of Game.Input.t
[@@deriving sexp]

let string_of_t v = Sexp.to_string @@ sexp_of_t v

let parse msg =
  print_endline @@ "trying to parse : \"" ^ msg ^ "\"";
  match Sexp.parse_string msg with
  | Ok sexp -> Some (t_of_sexp sexp)
  | Error _ -> None
