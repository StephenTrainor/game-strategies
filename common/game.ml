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

let all_equal_structural t l =
  match l with
  | [] -> true
  | head :: tail -> (
      let first_piece = Map.find t.board head in
      match first_piece with
      | None -> false
      | Some first_piece ->
          List.for_all tail ~f:(fun position ->
              match Map.find t.board position with
              | None -> false
              | Some other_piece -> Piece.equal first_piece other_piece))

let find_first_some (l : 'a option list) ~func =
  match List.findi l ~f:(fun _ x -> func x) with
  | None -> None
  | Some (_, element) -> element

let check_for_winning_piece t position : Piece.t option =
  let pieces_in_a_row_needed = Game_kind.win_length t.game_kind in
  let possible_directions = Position.offsets_no_duplicate_directions in
  let potential_winning_sequences =
    List.map possible_directions ~f:(fun direction ->
        List.init pieces_in_a_row_needed ~f:(fun number_of_applications ->
            Fn.apply_n_times ~n:number_of_applications direction position))
  in
  let equal_sequences =
    List.map potential_winning_sequences ~f:(fun sequence ->
        if all_equal_structural t sequence then
          Map.find t.board (List.hd_exn sequence)
        else None)
  in
  find_first_some equal_sequences ~func:Option.is_some

let return_winner t : Piece.t option =
  let positions = get_all_valid_positions t in
  List.fold positions
    ~init:(None : Piece.t option)
    ~f:(fun acc position ->
      match acc with
      | Some _piece -> acc
      | None -> check_for_winning_piece t position)
