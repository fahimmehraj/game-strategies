open! Core
open! Async
open! Game_strategies_common_lib

let diagonals =
  [
    [ (0, 14) ];
    [ (0, 13); (1, 14) ];
    [ (0, 12); (1, 13); (2, 14) ];
    [ (0, 11); (1, 12); (2, 13); (3, 14) ];
    [ (0, 10); (1, 11); (2, 12); (3, 13); (4, 14) ];
    [ (0, 9); (1, 10); (2, 11); (3, 12); (4, 13); (5, 14) ];
    [ (0, 8); (1, 9); (2, 10); (3, 11); (4, 12); (5, 13); (6, 14) ];
    [ (0, 7); (1, 8); (2, 9); (3, 10); (4, 11); (5, 12); (6, 13); (7, 14) ];
    [
      (0, 6); (1, 7); (2, 8); (3, 9); (4, 10); (5, 11); (6, 12); (7, 13); (8, 14);
    ];
    [
      (0, 5);
      (1, 6);
      (2, 7);
      (3, 8);
      (4, 9);
      (5, 10);
      (6, 11);
      (7, 12);
      (8, 13);
      (9, 14);
    ];
    [
      (0, 4);
      (1, 5);
      (2, 6);
      (3, 7);
      (4, 8);
      (5, 9);
      (6, 10);
      (7, 11);
      (8, 12);
      (9, 13);
      (10, 14);
    ];
    [
      (0, 3);
      (1, 4);
      (2, 5);
      (3, 6);
      (4, 7);
      (5, 8);
      (6, 9);
      (7, 10);
      (8, 11);
      (9, 12);
      (10, 13);
      (11, 14);
    ];
    [
      (0, 2);
      (1, 3);
      (2, 4);
      (3, 5);
      (4, 6);
      (5, 7);
      (6, 8);
      (7, 9);
      (8, 10);
      (9, 11);
      (10, 12);
      (11, 13);
      (12, 14);
    ];
    [
      (0, 1);
      (1, 2);
      (2, 3);
      (3, 4);
      (4, 5);
      (5, 6);
      (6, 7);
      (7, 8);
      (8, 9);
      (9, 10);
      (10, 11);
      (11, 12);
      (12, 13);
      (13, 14);
    ];
    [
      (0, 0);
      (1, 1);
      (2, 2);
      (3, 3);
      (4, 4);
      (5, 5);
      (6, 6);
      (7, 7);
      (8, 8);
      (9, 9);
      (10, 10);
      (11, 11);
      (12, 12);
      (13, 13);
      (14, 14);
    ];
    [
      (1, 0);
      (2, 1);
      (3, 2);
      (4, 3);
      (5, 4);
      (6, 5);
      (7, 6);
      (8, 7);
      (9, 8);
      (10, 9);
      (11, 10);
      (12, 11);
      (13, 12);
      (14, 13);
    ];
    [
      (2, 0);
      (3, 1);
      (4, 2);
      (5, 3);
      (6, 4);
      (7, 5);
      (8, 6);
      (9, 7);
      (10, 8);
      (11, 9);
      (12, 10);
      (13, 11);
      (14, 12);
    ];
    [
      (3, 0);
      (4, 1);
      (5, 2);
      (6, 3);
      (7, 4);
      (8, 5);
      (9, 6);
      (10, 7);
      (11, 8);
      (12, 9);
      (13, 10);
      (14, 11);
    ];
    [
      (4, 0);
      (5, 1);
      (6, 2);
      (7, 3);
      (8, 4);
      (9, 5);
      (10, 6);
      (11, 7);
      (12, 8);
      (13, 9);
      (14, 10);
    ];
    [
      (5, 0);
      (6, 1);
      (7, 2);
      (8, 3);
      (9, 4);
      (10, 5);
      (11, 6);
      (12, 7);
      (13, 8);
      (14, 9);
    ];
    [
      (6, 0); (7, 1); (8, 2); (9, 3); (10, 4); (11, 5); (12, 6); (13, 7); (14, 8);
    ];
    [ (7, 0); (8, 1); (9, 2); (10, 3); (11, 4); (12, 5); (13, 6); (14, 7) ];
    [ (8, 0); (9, 1); (10, 2); (11, 3); (12, 4); (13, 5); (14, 6) ];
    [ (9, 0); (10, 1); (11, 2); (12, 3); (13, 4); (14, 5) ];
    [ (10, 0); (11, 1); (12, 2); (13, 3); (14, 4) ];
    [ (11, 0); (12, 1); (13, 2); (14, 3) ];
    [ (12, 0); (13, 1); (14, 2) ];
    [ (13, 0); (14, 1) ];
    [ (14, 0) ];
    [ (0, 0) ];
    [ (0, 1); (1, 0) ];
    [ (0, 2); (1, 1); (2, 0) ];
    [ (0, 3); (1, 2); (2, 1); (3, 0) ];
    [ (0, 4); (1, 3); (2, 2); (3, 1); (4, 0) ];
    [ (0, 5); (1, 4); (2, 3); (3, 2); (4, 1); (5, 0) ];
    [ (0, 6); (1, 5); (2, 4); (3, 3); (4, 2); (5, 1); (6, 0) ];
    [ (0, 7); (1, 6); (2, 5); (3, 4); (4, 3); (5, 2); (6, 1); (7, 0) ];
    [ (0, 8); (1, 7); (2, 6); (3, 5); (4, 4); (5, 3); (6, 2); (7, 1); (8, 0) ];
    [
      (0, 9);
      (1, 8);
      (2, 7);
      (3, 6);
      (4, 5);
      (5, 4);
      (6, 3);
      (7, 2);
      (8, 1);
      (9, 0);
    ];
    [
      (0, 10);
      (1, 9);
      (2, 8);
      (3, 7);
      (4, 6);
      (5, 5);
      (6, 4);
      (7, 3);
      (8, 2);
      (9, 1);
      (10, 0);
    ];
    [
      (0, 11);
      (1, 10);
      (2, 9);
      (3, 8);
      (4, 7);
      (5, 6);
      (6, 5);
      (7, 4);
      (8, 3);
      (9, 2);
      (10, 1);
      (11, 0);
    ];
    [
      (0, 12);
      (1, 11);
      (2, 10);
      (3, 9);
      (4, 8);
      (5, 7);
      (6, 6);
      (7, 5);
      (8, 4);
      (9, 3);
      (10, 2);
      (11, 1);
      (12, 0);
    ];
    [
      (0, 13);
      (1, 12);
      (2, 11);
      (3, 10);
      (4, 9);
      (5, 8);
      (6, 7);
      (7, 6);
      (8, 5);
      (9, 4);
      (10, 3);
      (11, 2);
      (12, 1);
      (13, 0);
    ];
    [
      (0, 14);
      (1, 13);
      (2, 12);
      (3, 11);
      (4, 10);
      (5, 9);
      (6, 8);
      (7, 7);
      (8, 6);
      (9, 5);
      (10, 4);
      (11, 3);
      (12, 2);
      (13, 1);
      (14, 0);
    ];
    [
      (1, 14);
      (2, 13);
      (3, 12);
      (4, 11);
      (5, 10);
      (6, 9);
      (7, 8);
      (8, 7);
      (9, 6);
      (10, 5);
      (11, 4);
      (12, 3);
      (13, 2);
      (14, 1);
    ];
    [
      (2, 14);
      (3, 13);
      (4, 12);
      (5, 11);
      (6, 10);
      (7, 9);
      (8, 8);
      (9, 7);
      (10, 6);
      (11, 5);
      (12, 4);
      (13, 3);
      (14, 2);
    ];
    [
      (3, 14);
      (4, 13);
      (5, 12);
      (6, 11);
      (7, 10);
      (8, 9);
      (9, 8);
      (10, 7);
      (11, 6);
      (12, 5);
      (13, 4);
      (14, 3);
    ];
    [
      (4, 14);
      (5, 13);
      (6, 12);
      (7, 11);
      (8, 10);
      (9, 9);
      (10, 8);
      (11, 7);
      (12, 6);
      (13, 5);
      (14, 4);
    ];
    [
      (5, 14);
      (6, 13);
      (7, 12);
      (8, 11);
      (9, 10);
      (10, 9);
      (11, 8);
      (12, 7);
      (13, 6);
      (14, 5);
    ];
    [
      (6, 14);
      (7, 13);
      (8, 12);
      (9, 11);
      (10, 10);
      (11, 9);
      (12, 8);
      (13, 7);
      (14, 6);
    ];
    [ (7, 14); (8, 13); (9, 12); (10, 11); (11, 10); (12, 9); (13, 8); (14, 7) ];
    [ (8, 14); (9, 13); (10, 12); (11, 11); (12, 10); (13, 9); (14, 8) ];
    [ (9, 14); (10, 13); (11, 12); (12, 11); (13, 10); (14, 9) ];
    [ (10, 14); (11, 13); (12, 12); (13, 11); (14, 10) ];
    [ (11, 14); (12, 13); (13, 12); (14, 11) ];
    [ (12, 14); (13, 13); (14, 12) ];
    [ (13, 14); (14, 13) ];
    [ (14, 14) ];
  ]
  |> List.map ~f:(fun diag ->
         List.map diag ~f:(fun (row, column) -> { Position.row; column }))

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
  let preferred_positions =
    List.filter all_positions ~f:(fun pos ->
        let neighbors =
          List.map Position.all_offsets ~f:(fun direction -> direction pos)
          |> List.filter ~f:(fun nei ->
                 Position.in_bounds nei ~game_kind:game.game_kind)
          |> List.filter ~f:(fun nei ->
                 Option.is_some (Map.find game.board nei))
        in
        Option.is_none (Map.find game.board pos)
        && not (List.is_empty neighbors))
  in
  match preferred_positions with
  | [] ->
      List.filter all_positions ~f:(fun pos ->
          Option.is_none (Map.find game.board pos))
  | preferred_positions -> preferred_positions

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
                List.exists Position.all_offsets ~f:(fun direction ->
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

let get_streaks (game : Game.t) (player : Piece.t) =
  let weight piece = if Piece.equal piece player then 1. else -1. in
  let score_streak streak = Float.int_pow 2. streak *. weight player in

  let rows =
    Array.init (Game_kind.board_length game.game_kind) ~f:(fun row ->
        Array.init (Game_kind.board_length game.game_kind) ~f:(fun column ->
            Map.find game.board { row; column }))
  in

  let horizontal_score =
    Array.fold rows ~init:0. ~f:(fun score row ->
        let (_current, streak, row_score) : Piece.t option * int * float =
          Array.fold row ~init:(None, 0, 0.)
            ~f:(fun (current, streak, score) piece ->
              match (current, piece) with
              | None, None | _, None -> (None, 0, score)
              | None, Some piece -> (Some piece, 1, score)
              | Some current, Some piece -> (
                  match Piece.equal current piece with
                  | false -> (Some piece, 1, score +. score_streak streak)
                  | true -> (Some piece, streak + 1, score)))
        in
        let row_score = row_score +. score_streak streak in
        score +. row_score)
  in

  let columns =
    Array.init (Game_kind.board_length game.game_kind) ~f:(fun row ->
        Array.init (Game_kind.board_length game.game_kind) ~f:(fun column ->
            Map.find game.board { row = column; column = row }))
  in

  let vertical_score =
    Array.fold columns ~init:0. ~f:(fun score row ->
        let (_current, streak, row_score) : Piece.t option * int * float =
          Array.fold row ~init:(None, 0, 0.)
            ~f:(fun (current, streak, score) piece ->
              match (current, piece) with
              | None, None | _, None -> (None, 0, score)
              | None, Some piece -> (Some piece, 1, score)
              | Some current, Some piece -> (
                  match Piece.equal current piece with
                  | false -> (Some piece, 1, score +. score_streak streak)
                  | true -> (Some piece, streak + 1, score)))
        in
        let row_score = row_score +. score_streak streak in
        score +. row_score)
  in
  (* 0, 0 *)
  (* 0, 1 *)
  (* 1, 0 *)
  (* 0, 2 *)
  (* 2, 0*)

  let diagonals =
    List.map diagonals ~f:(fun diag ->
        List.map diag ~f:(fun pos -> Map.find game.board pos))
  in
  let diagonal_score =
    List.fold diagonals ~init:0. ~f:(fun score row ->
        let (_current, streak, row_score) : Piece.t option * int * float =
          List.fold row ~init:(None, 0, 0.)
            ~f:(fun (current, streak, score) piece ->
              match (current, piece) with
              | None, None | _, None -> (None, 0, score)
              | None, Some piece -> (Some piece, 1, score)
              | Some current, Some piece -> (
                  match Piece.equal current piece with
                  | false -> (Some piece, 1, score +. score_streak streak)
                  | true -> (Some piece, streak + 1, score)))
        in
        let row_score = row_score +. score_streak streak in
        score +. row_score)
  in
  let result = (horizontal_score +. vertical_score +. diagonal_score) *. 10000000. in
  print_s [%sexp (result : float)];
  result

let score ~(node : Game.t) ~(us : Piece.t) =
  if Game_kind.equal Game_kind.Tic_tac_toe node.game_kind then 0.
  else
    let _board = node.board in
    (* let all_pos = all_positions node in
    let centering =
      List.fold all_pos ~init:0 ~f:(fun total pos ->
          let closeness = 98 - Position.from_center pos in
          match Map.find board pos with
          | Some piece ->
              if Piece.equal us piece then total + closeness
              else total - closeness
          | None -> total)
    in *)

    get_streaks node us

let rec minimax (game : Game.t) (piece : Piece.t) (depth : int) alpha beta =
  match evaluate game with
  | Game_over { winner = Some winner } ->
      if Piece.equal winner piece then Float.infinity else Float.neg_infinity
  | Game_over _ -> 0.
  | Illegal_move -> failwith "Can't evaluate illegal move"
  | Game_continues ->
      if depth = 0 then score ~node:game ~us:piece
      else
        let next_positions = available_moves game in
        List.fold_until next_positions
          ~init:(Float.neg_infinity, alpha)
          ~f:(fun (current_max, alpha) pos ->
            let result =
              Float.max
                (-1.
                *. minimax
                     (Game.set_piece game pos piece)
                     (Piece.flip piece) (depth - 1) (-1. *. beta) (-1. *. alpha)
                )
                current_max
            in
            let alpha = Float.max alpha result in
            if Float.(alpha >= beta) then Stop result
            else Continue (result, alpha))
          ~finish:(fun (result, _) -> result)

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  match
    available_moves game
    |> List.map ~f:(fun move ->
           ( move,
             -1.
             *. minimax
                  (Game.set_piece game move you_play)
                  (Piece.flip you_play) 2 Float.neg_infinity Float.infinity ))
    |> List.max_elt ~compare:(fun (_, score1) (_, score2) ->
           Float.compare score1 score2)
  with
  | Some (move, _maximum) ->
      (* print_s [%sexp (_maximum : float)]; *)
      move
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
