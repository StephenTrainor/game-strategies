open! Core
open! Async

type t = { game_kind : Game_kind.t; board : Piece.t Position.Map.t }
[@@deriving sexp_of, bin_io]

let empty game_kind = { game_kind; board = Position.Map.empty }

let set_piece t position piece =
  { t with board = Map.set t.board ~key:position ~data:piece }

let get_all_valid_positions t : Position.t list =
  let board_length = Game_kind.board_length t.game_kind in
  List.init (board_length * board_length) ~f:(fun i ->
      let column = i % board_length in
      let row = i / board_length in
      ({ row; column } : Position.t))
