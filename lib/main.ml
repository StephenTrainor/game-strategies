open! Core
open! Async
open! Game_strategies_common_lib

let print_positions l =
  List.iter l ~f:(fun li -> Position.to_string li |> print_endline)

let x_float_infinity = 999999.0;;
let o_float_infinity = -999999.0;;

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

(* Exercise 6 *)
let available_moves_that_do_not_immediately_lose ~(game : Game.t)
    ~(player_piece : Piece.t) : Position.t list =
  let available_moves = available_moves game in
  List.filter available_moves ~f:(fun move ->
      let potential_game = Game.set_piece game move player_piece in
      match winning_moves ~me:(Piece.flip player_piece) potential_game with
      | [] -> true
      | _ :: _ -> false)

let exercise_six =
  Command.async
    ~summary:
      "Exercise 6: Are there moves that ensure that the opponent will not win \
       right after?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let available_moves_that_do_not_immediately_lose =
         available_moves_that_do_not_immediately_lose ~player_piece:piece
           ~game:non_win
       in
       print_s
         [%sexp
           (available_moves_that_do_not_immediately_lose : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
      ("six", exercise_six);
    ]

let%expect_test "not_immediately_lose_non_win" =
  available_moves_that_do_not_immediately_lose ~game:non_win ~player_piece:X
  |> print_positions;
  [%expect
    {|
  ((row 0) (column 1))
  ((row 0) (column 2))
  ((row 1) (column 2))
  ((row 2) (column 1))|}];
  return ()

let%expect_test "not_immediately_lose_block_win" =
  available_moves_that_do_not_immediately_lose ~game:non_win ~player_piece:O
  |> print_positions;
  [%expect {|((row 1) (column 1))|}];
  return ()

let available_moves_adjacent_to_existing ?(depth = 1) (game : Game.t) =
  let keys = Map.key_set game.board in
  let adjacent_moves = ref Position.Set.empty in
  let rec loop l n =
    match n with
    | 0 -> ()
    | _ ->
        Set.iter l ~f:(fun key ->
            List.iter Position.all_offsets ~f:(fun function_offset ->
                let neighbor = function_offset key in
                if
                  (not (Map.mem game.board neighbor))
                  && Position.in_bounds neighbor ~game_kind:game.game_kind
                then adjacent_moves := Set.add !adjacent_moves neighbor));
        loop !adjacent_moves (n - 1)
  in
  loop keys depth;
  match Set.length !adjacent_moves with
  | 0 ->
      let middle = Game_kind.board_length game.game_kind / 2 in
      [ { Position.row = middle; column = middle } ]
  | _ -> Set.to_list !adjacent_moves

let is_winning_infinite piece score =
  match piece with 
  | Piece.X -> if Float.(>) score 10000.0 then true else false
  | O -> if Float.(<) score (-10000.0) then true else false

let is_winning_infinite_helper piece pair = is_winning_infinite piece (fst pair)

let get_best_moves game =
  let _, critical_positions = Game.score game in
  match critical_positions with
  | [] -> available_moves_adjacent_to_existing game ~depth:1
  | _ :: _ -> critical_positions

let new_games_from_neighbors (game : Game.t) piece =
  get_best_moves game
  |> List.map ~f:(fun neighbor -> Game.set_piece game neighbor piece)

let new_games_from_critical_positions game positions piece =
  List.map positions ~f:(fun neighbor -> Game.set_piece game neighbor piece)

(* iterative deepening *)
let rec minimax ~game ~depth ~current_player alpha beta finish_by =
  match depth with
  | 0 -> fst (Game.score game)
  | _ -> (
      if Time_ns_unix.( < ) finish_by (Time_ns_unix.now ()) then
        match current_player with
        | Piece.X -> o_float_infinity
        | O -> x_float_infinity
      else
        let score, critical_positions = Game.score game in
        let critical_games =
          match critical_positions with
          | [] -> new_games_from_neighbors game current_player
          | _ :: _ ->
              new_games_from_critical_positions game critical_positions
                current_player
        in
        match is_winning_infinite current_player score with
        | true -> score
        | false -> (
            let bias = 0.9 in
            match current_player with
            | Piece.X ->
                let rec loop l acc alpha beta =
                  match l with
                  | [] -> acc
                  | new_game :: tail ->
                      let score =
                        minimax ~game:new_game ~depth:(depth - 1)
                          ~current_player:(Piece.flip current_player)
                          alpha beta finish_by
                      in
                      let acc = Float.max acc (bias *. score) in
                      if Float.( >= ) acc beta then acc
                      else loop tail acc (Float.max alpha acc) beta
                in
                loop critical_games o_float_infinity alpha beta
            | O ->
                let rec loop l acc alpha beta =
                  match l with
                  | [] -> acc
                  | new_game :: tail ->
                      let score =
                        minimax ~game:new_game ~depth:(depth - 1)
                          ~current_player:(Piece.flip current_player)
                          alpha beta finish_by
                      in
                      let acc = Float.min acc (bias *. score) in
                      if Float.( <= ) acc alpha then acc
                      else loop tail acc alpha (Float.min beta acc)
                in
                loop critical_games x_float_infinity alpha beta))

let max_tuple l =
  List.max_elt l ~compare:(fun (a, _) (b, _) -> Float.compare a b)

let min_tuple l =
  List.min_elt l ~compare:(fun (a, _) (b, _) -> Float.compare a b)

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  match Map.length game.board with
  | 0 ->
      let middle = Game_kind.board_length game.game_kind / 2 in
      { row = middle; column = middle }
  | _ -> (
      let winning_moves = winning_moves ~me:you_play game in
      match winning_moves with
      | [] -> (
          let start_time = Time_ns_unix.now () in
          let finish_by =
            Time_ns_unix.add start_time (Time_ns_unix.Span.of_ms 9900.0)
          in
          let rec depth_loop current_depth best_so_far =
            if
              Float.( >= )
                (Time_ns_unix.Span.to_ms
                   (Time_ns_unix.diff (Time_ns_unix.now ()) start_time))
                9800.0
            then (
              print_endline (string_of_int current_depth);
              best_so_far)
            else
              let get_best_moves = get_best_moves game in
              match current_depth <= 225 with
              | false -> best_so_far
              | true -> (
                  let minimaxed_moves =
                    List.map get_best_moves ~f:(fun move ->
                        ( minimax
                            ~game:(Game.set_piece game move you_play)
                            ~depth:current_depth
                            ~current_player:(Piece.flip you_play)
                            o_float_infinity x_float_infinity finish_by,
                          move ))
                  in
                  let tuple_fn_to_use =
                    match you_play with Piece.O -> min_tuple | X -> max_tuple
                  in
                  match tuple_fn_to_use minimaxed_moves with
                  | None -> depth_loop (current_depth + 1) best_so_far
                  | Some pair -> ( 
                      match is_winning_infinite_helper you_play pair with
                      | true -> Some pair
                      | false -> depth_loop (current_depth + 1) (Some pair)
                      ))
          in
          match depth_loop 1 None with
          | None -> List.random_element_exn (available_moves game)
          | Some (_, best_position) -> best_position)
      | win :: _ -> win)
