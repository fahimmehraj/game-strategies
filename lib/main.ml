open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

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

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

(* Use String.concat seperator logic to clean it up *)
let print_game (game : Game.t) =
  let board_length = Game_kind.board_length game.game_kind in
  let num_dashes = (board_length * 4) - 3 in
  let row_sep = "\n" ^ String.make num_dashes '-' ^ "\n" in

  List.init board_length ~f:(fun row ->
      List.init board_length ~f:(fun column ->
          match Map.find game.board { Position.row; column } with
          | Some piece -> Piece.to_string piece
          | None -> " ")
      |> String.concat ~sep:" | ")
  |> String.concat ~sep:row_sep |> print_endline

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

let all_positions (game : Game.t) : Position.t list =
  let one_row = List.init (Game_kind.board_length game.game_kind) ~f:Fn.id in
  List.cartesian_product one_row one_row
  |> List.map ~f:(fun (x, y) -> { Position.row = x; column = y })

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let all_positions = all_positions game in
  List.filter all_positions ~f:(fun key ->
      Option.is_none (Map.find game.board key))

let%expect_test "available_moves" =
  print_s [%sexp (available_moves win_for_x : Position.t list)];
  [%expect {|
  ()
  |}];
  print_s [%sexp (available_moves non_win : Position.t list)];
  [%expect
    {|
  (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
   ((row 1) (column 2)) ((row 2) (column 1)))
  |}];
  return ()

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let get_piece position = Map.find game.board position in

  let rec is_winnable ~target position depth direction =
    match
      (depth >= Game_kind.win_length game.game_kind, get_piece position)
    with
    | true, _ -> true
    | false, None -> false
    | false, Some piece -> (
        match Piece.equal target piece with
        | false -> false
        | true -> is_winnable ~target (direction position) (depth + 1) direction
        )
  in
  (* check if all pieces in bound *)
  if
    Map.existsi game.board ~f:(fun ~key ~data:_ ->
        not (Position.in_bounds key ~game_kind:game.game_kind))
  then Evaluation.Illegal_move
  else
    let all_positions = all_positions game in
    match
      List.find_map all_positions ~f:(fun position ->
          match get_piece position with
          | None -> None
          | Some target ->
              let win_condition_exists =
                List.exists Position.half_offsets ~f:(fun direction ->
                    is_winnable ~target position 0 direction)
              in
              if win_condition_exists then Some target else None)
    with
    | Some winner -> Evaluation.Game_over { winner = Some winner }
    | None -> (
        match List.is_empty (available_moves game) with
        | true -> Evaluation.Game_over { winner = None }
        | false -> Evaluation.Game_continues)

let%expect_test "evaluate_win_for_x" =
  let result = evaluate win_for_x in
  print_s [%sexp (result : Evaluation.t)];
  [%expect {| (Game_over (winner (X))) |}];
  return ()

let%expect_test "evaluate_continuing_game" =
  let result = evaluate non_win in
  print_s [%sexp (result : Evaluation.t)];
  [%expect {| Game_continues |}];
  return ()

let%expect_test "evaluate_illegal_board" =
  let illegal_game = init_game [ ({ row = 8; column = 2 }, X) ] in
  print_s [%sexp (evaluate illegal_game : Evaluation.t)];
  [%expect {| Illegal_move |}];
  return ()

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  available_moves game
  |> List.filter ~f:(fun move ->
         match evaluate (Game.set_piece game move me) with
         | Evaluation.Game_over { winner = Some winner } ->
             Piece.equal winner me
         | _ -> false)

let%expect_test "winning_moves_with_three_spots" =
  let data =
    init_game
      [
        ({ row = 0; column = 0 }, X);
        ({ row = 0; column = 1 }, X);
        ({ row = 1; column = 0 }, X);
        ({ row = 1; column = 2 }, O);
        ({ row = 2; column = 1 }, O);
        ({ row = 2; column = 2 }, X);
      ]
  in
  print_s [%sexp (winning_moves ~me:Piece.X data : Position.t list)];
  [%expect
    {| (((row 0) (column 2)) ((row 1) (column 1)) ((row 2) (column 0))) |}];
  return ()

let%expect_test "winning_moves_full_board" =
  let full_board = win_for_x in
  print_s [%sexp (winning_moves ~me:Piece.O full_board : Position.t list)];
  [%expect {| () |}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  available_moves game
  |> List.filter ~f:(fun move ->
         match
           winning_moves ~me:(Piece.flip me) (Game.set_piece game move me)
         with
         | [] -> false
         | _ -> true)

let%expect_test "everywhere_but_one_winning_move" =
  let data =
    init_game
      [
        ({ row = 0; column = 0 }, X);
        ({ row = 1; column = 0 }, X);
        ({ row = 0; column = 2 }, O);
      ]
  in
  print_s [%sexp (losing_moves ~me:Piece.O data : Position.t list)];
  [%expect
    {|
      (((row 0) (column 1)) ((row 1) (column 1)) ((row 1) (column 2))
       ((row 2) (column 1)) ((row 2) (column 2))) |}];
  return ()

(* Exercise 6 *)
let available_moves_that_do_not_immediately_lose ~(me : Piece.t) (game : Game.t)
    =
  available_moves game
  |> List.filter ~f:(fun move ->
         match
           winning_moves ~me:(Piece.flip me) (Game.set_piece game move me)
         with
         | [] -> true
         | _ -> false)

let%expect_test "available_moves_that_do_not_immediately_lose" =
  let data =
    init_game
      [
        ({ row = 0; column = 0 }, X);
        ({ row = 1; column = 0 }, X);
        ({ row = 0; column = 1 }, O);
      ]
  in
  let moves = available_moves_that_do_not_immediately_lose ~me:O data in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| (((row 2) (column 0))) |}];
  return ()

let rec minimax (game : Game.t) (piece : Piece.t) (depth : int) =
  match evaluate game with
  | Game_over { winner = Some winner } ->
      if Piece.equal winner piece then Float.infinity else Float.neg_infinity
  | Game_over _ -> 0.
  | Illegal_move -> failwith "Can't evaluate illegal move"
  | Game_continues -> (
      if depth = 0 then 0.
      else
        let next_positions = available_moves game in
        match
          List.map next_positions ~f:(fun pos ->
             (-1.) *. minimax (Game.set_piece game pos piece) (Piece.flip piece) (depth - 1))
          |> List.max_elt ~compare:Float.compare
        with
        | Some maximum -> maximum
        | None -> failwith "Available move always exists")

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  match
    available_moves game
    |> List.map ~f:(fun move ->
           (move, minimax (Game.set_piece game move you_play) you_play 100))
    |> List.max_elt ~compare:(fun (_, score1) (_, score2) ->
           Float.compare score1 score2)
  with
  | Some (move, _maximum) -> move
  | None -> failwith "Move should exist"

let%expect_test "make_move_easy_win" =
  let data =
    init_game
      [
        ({ row = 0; column = 0 }, X);
        ({ row = 1; column = 0 }, X);
        ({ row = 0; column = 1 }, O);
        ({ row = 1; column = 1 }, O);
      ]
  in
  let x_next_move = make_move ~game:data ~you_play:X in
  print_s [%sexp (x_next_move : Position.t)];
  [%expect {| ((row 2) (column 0)) |}];
  return ()

let%expect_test "make_move_block_win" =
  let data =
    init_game
      [
        ({ row = 0; column = 0 }, X);
        ({ row = 0; column = 2 }, X);
        ({ row = 1; column = 1 }, O);
      ]
  in
  let o_next_move = make_move ~game:data ~you_play:O in
  print_s [%sexp (o_next_move : Position.t)];
  [%expect {| ((row 0) (column 1)) |}];
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

let exercise_five =
  Command.async ~summary:"Exercise 5: Make a move"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let data =
         init_game
           [
             ({ row = 0; column = 0 }, X);
             ({ row = 1; column = 0 }, X);
             ({ row = 0; column = 1 }, O);
             ({ row = 1; column = 1 }, O);
           ]
       in
       let x_next_move = make_move ~game:data ~you_play:piece in
       print_s [%sexp (x_next_move : Position.t)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
      ("five", exercise_five);
    ]
