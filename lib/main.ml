open! Core
open! Async
open! Game_strategies_common_lib

let print_positions l =
  List.iter l ~f:(fun li -> Position.to_string li |> print_endline)

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let draw =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, O);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let almost_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let print_game (game : Game.t) =
  let board_length = Game_kind.board_length game.game_kind in
  let row_of_dashes = String.make ((board_length * 4) - 3) '-' in
  let output =
    List.init board_length ~f:(fun row ->
        List.init board_length ~f:(fun column ->
            let player_piece =
              Map.find game.board ({ column; row } : Position.t)
            in
            match player_piece with
            | Some piece -> Piece.to_string piece
            | None -> " ")
        |> String.concat ~sep:" | ")
    |> String.concat ~sep:("\n" ^ row_of_dashes ^ "\n")
  in
  print_endline output

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   | 
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  Game.get_all_valid_positions game
  |> List.filter ~f:(fun position ->
         match Map.find game.board position with
         | None -> true
         | Some _piece -> false)

let%expect_test "no_available_moves" =
  print_positions (available_moves win_for_x);
  [%expect {||}];
  return ()

let%expect_test "some_available_moves" =
  print_positions (available_moves non_win);
  [%expect
    {|
  ((row 0) (column 1))
  ((row 0) (column 2))
  ((row 1) (column 1))
  ((row 1) (column 2))
  ((row 2) (column 1))
  |}];
  return ()

let%expect_test "one_available_move" =
  print_positions (available_moves almost_win);
  [%expect {|((row 1) (column 2))|}];
  return ()

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let keys = Map.key_set game.board in
  if
    Set.exists keys ~f:(fun key ->
        not (Position.in_bounds key ~game_kind:game.game_kind))
  then Evaluation.Illegal_move
  else
    match Game.return_winner game with
    | None ->
        let number_of_available_moves = available_moves game |> List.length in
        if number_of_available_moves <> 0 then Evaluation.Game_continues
        else Evaluation.Game_over { winner = None }
    | Some piece -> Evaluation.Game_over { winner = Some piece }

let%expect_test "evaluate_win_for_x" =
  evaluate win_for_x |> Evaluation.sexp_of_t |> print_s;
  [%expect {|
    (Game_over (winner (X)))|}];
  return ()

let%expect_test "evaluate_almost_win" =
  evaluate almost_win |> Evaluation.sexp_of_t |> print_s;
  [%expect {|
     Game_continues|}];
  return ()

let%expect_test "evaluate_draw" =
  evaluate draw |> Evaluation.sexp_of_t |> print_s;
  [%expect {|
    (Game_over (winner ()))|}];
  return ()

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let available_moves = available_moves game in
  List.filter available_moves ~f:(fun position ->
      let new_game = Game.set_piece game position me in
      match evaluate new_game with
      | Evaluation.Game_over { winner = Some _ } -> true
      | Game_over { winner = None } | Game_continues | Illegal_move -> false)

let%expect_test "winning_moves_almost_win" =
  winning_moves ~me:X almost_win |> print_positions;
  [%expect {|
  ((row 1) (column 2))|}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let available_moves = available_moves game in
  let opponent_winning_moves =
    Position.Set.of_list (winning_moves ~me:(Piece.flip me) game)
  in
  List.filter available_moves ~f:(fun move ->
      not (Set.mem opponent_winning_moves move))

let%expect_test "losing_moves_game_over" =
  losing_moves ~me:X win_for_x |> print_positions;
  [%expect {||}];
  return ()

let%expect_test "losing_moves_non_win" =
  losing_moves ~me:O non_win |> print_positions;
  [%expect
    {|
    ((row 0) (column 1))
    ((row 0) (column 2))
    ((row 1) (column 2))
    ((row 2) (column 1))
    |}];
  return ()

let%expect_test "losing_moves_almost_win_for_x" =
  losing_moves ~me:X almost_win |> print_positions;
  [%expect {||}];
  return ()

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  ignore game;
  ignore you_play;
  failwith "Implement me!"
