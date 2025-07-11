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

let infinity piece =
  match piece with Piece.X -> Float.infinity | O -> Float.neg_infinity

let score_sequence piece consecutive =
  let consecutive = float_of_int consecutive in
  match piece with
  | None -> 0.0
  | Some Piece.X -> Float.square consecutive
  | Some O -> Float.square consecutive *. -1.

let score_list t l =
  let win_length = Game_kind.win_length t.game_kind in
  let rec scoring_loop l prev consecutive total_score =
    if consecutive = win_length then infinity (Option.value_exn prev)
    else
      match l with
      | [] -> total_score +. score_sequence prev consecutive
      | head :: tail -> (
          let next = Map.find t.board head in
          match (prev, next) with
          | Some Piece.X, Some Piece.X | Some O, Some O ->
              scoring_loop tail prev (consecutive + 1) total_score
          | _ ->
              scoring_loop tail next 1
                (total_score +. score_sequence prev consecutive))
  in
  scoring_loop l None 0 0.0

let sum_float_list = List.fold ~init:0.0 ~f:(fun acc rest -> acc +. rest)

let score t =
  let board_length = Game_kind.board_length t.game_kind in
  let row_total =
    List.init board_length ~f:(fun row ->
        let list_of_rows =
          List.init board_length ~f:(fun column -> { Position.row; column })
        in
        score_list t list_of_rows)
    |> sum_float_list
  in
  let column_total =
    List.init board_length ~f:(fun column ->
        let list_of_columns =
          List.init board_length ~f:(fun row -> { Position.row; column })
        in
        score_list t list_of_columns)
    |> sum_float_list
  in
  let diagonals = 2 * board_length - 1 in
  let backward_diagonal_total = 
    List.init diagonals ~f:(fun row -> 
      let diagonal = match row >= board_length with 
      | true ->  List.init (row + 1 - board_length) ~f:(fun column -> {Position.row = column; column = ((2 * board_length) - row + column - 1)})
      | false -> List.init (board_length - row) ~f:(fun column -> {Position.row = (row + column); column})
      in
      score_list t diagonal
    ) |> sum_float_list
  in
  let forward_diagonal_total = 
    List.init diagonals ~f:(fun row -> 
      let diagonal = match row >= board_length with 
      | true ->  List.init (row + 1 - board_length) ~f:(fun column -> { Position.row = (2 * board_length) - row + column - 1; column = (board_length - column - 1)})
      | false -> List.init (row + 1) ~f:(fun column -> { Position.row = (row - column); column})
      in
      score_list t diagonal
    ) |> sum_float_list
  in
  row_total +. column_total +. backward_diagonal_total +. forward_diagonal_total

let return_winner t : Piece.t option =
  let score = score t in
  match Float.is_finite score with
  | true -> None
  | false -> if Float.( > ) score 0.0 then Some Piece.X else Some O
