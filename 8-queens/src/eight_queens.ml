open Printf


(* ------------------------------------------------------------------------- *
 * Types
 * ------------------------------------------------------------------------- *)

type square_state =
  Queen | Empty


type direction =
  North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


(* ------------------------------------------------------------------------- *
 * Utils
 * ------------------------------------------------------------------------- *)

let (|>) x f = f x


let seq n_from n_to step =
  let rec seq = function
    | n, acc' when n = n_to -> n::acc'
    | n, acc' -> seq ((n + step), n::acc')
  in
  seq (n_from, [])


let rep a times =
  let rec rep = function
    | 0, acc' -> acc'
    | n, acc' -> rep (n - 1, a::acc')
  in
  rep (times, [])


(* ------------------------------------------------------------------------- *
 * Core
 * ------------------------------------------------------------------------- *)

let directions_all : direction list  =
  [North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest]


let directions_diagonal =
  [       NorthEast;       SouthEast;        SouthWest;       NorthWest]


let weight_of_state = function
  | Empty -> 0
  | Queen -> 1


let char_of_state = function
  | Empty -> '-'
  | Queen -> 'Q'


let str_of_dir = function
  | North     -> "N "
  | NorthEast -> "NE"
  | East      -> "E "
  | SouthEast -> "SE"
  | South     -> "S "
  | SouthWest -> "SW"
  | West      -> "W "
  | NorthWest -> "NW"


let print_square (state : square_state) : unit =
  printf "%c " (char_of_state state)


let print_board board =
  Array.iter
  (
    fun row ->
      Array.iter
      (print_square)
      row;
      print_newline ()
  )
  board


let vector_fwd  = seq   7    1  (-1)
let vector_flat = rep   0    7
let vector_rev  = seq (-7) (-1)   1


let offsets_N  = List.combine vector_flat vector_rev
let offsets_NE = List.combine vector_fwd  vector_rev
let offsets_E  = List.combine vector_fwd  vector_flat
let offsets_SE = List.combine vector_fwd  vector_fwd
let offsets_S  = List.combine vector_flat vector_fwd
let offsets_SW = List.combine vector_rev  vector_fwd
let offsets_W  = List.combine vector_rev  vector_flat
let offsets_NW = List.combine vector_rev  vector_rev


let offsets_of_dir = function
  | North     -> offsets_N
  | NorthEast -> offsets_NE
  | East      -> offsets_E
  | SouthEast -> offsets_SE
  | South     -> offsets_S
  | SouthWest -> offsets_SW
  | West      -> offsets_W
  | NorthWest -> offsets_NW


let is_onside (x, y) =
  x >= 0 && x < 8 &&
  y >= 0 && y < 8


let view_in_dir x y direction =
  let offsets = offsets_of_dir direction in
  List.filter (is_onside) (List.map (fun (xo, yo) -> x + xo, y + yo) offsets)


let print_board_diagnostics board directions =
  Array.iteri
  (
    fun yi row ->
      Array.iteri
      (
        fun xi square ->
          match square with
          | Queen ->

            printf "(X: %d, Y: %d, STATE: %c)\n" xi yi (char_of_state square);

            List.iter
            (
              fun dir ->
                let offsets = offsets_of_dir dir in
                let view = view_in_dir xi yi dir in
                let view_weights =
                  List.map (fun (x, y) -> weight_of_state board.(y).(x)) view
                in

                printf "%s-O : " (str_of_dir dir);
                List.iter
                (
                  fun (x, y) ->
                    printf "%d,%d " x y
                )
                offsets;
                print_newline ();

                printf "%s-C : " (str_of_dir dir);
                List.iter
                (
                  fun (x, y) ->
                    printf "%d,%d " x y
                )
                view;
                print_newline ();

                printf "%s-S : " (str_of_dir dir);
                List.iter
                (
                  fun (x, y) ->
                    printf " %c  " (char_of_state board.(y).(x))
                )
                view;
                print_newline ();

                printf "%s-W : " (str_of_dir dir);
                List.iter
                (
                  fun (x, y) ->
                    printf " %d  " (weight_of_state board.(y).(x))
                )
                view;
                print_newline ();

                printf "%s-WT: " (str_of_dir dir);
                let view_weight = List.fold_left (+) 0 view_weights in
                printf " %d" view_weight;
                print_newline ();
            )
            directions;
            print_newline ();

          | Empty -> ()
      )
      row;
      print_newline ()
  )
  board;
  print_newline ();
  print_newline ()


let weight_of_position board x y =
  let views = List.map (view_in_dir x y) directions_all in
  let weights =
    List.map
    (
      List.fold_left
      (fun acc (xv, yv) -> (weight_of_state board.(yv).(xv)) + acc)
      0
    )
    views
  in
  List.fold_left (+) 0 weights


let print_board_weighted board =
  Array.iteri
  (
    fun y row ->
      Array.iteri
      (
        fun x state -> match state with
        | Queen -> printf "%d " (weight_of_position board x y)
        | Empty -> print_string "- "
      )
      row;
      print_newline ()
  )
  board


let board_weight board =
  let board_weighted =
    Array.mapi
    (
      fun y row ->
        Array.mapi
        (
          fun x state -> match state with
          | Queen -> weight_of_position board x y
          | Empty -> 0
        )
        row
    )
    board
  in
  let weights = Array.map (Array.fold_left (+) 0) board_weighted in
  Array.fold_left (+) 0 weights


let init_board () =
  let board = Array.make_matrix 8 8 Empty
  and num_queens = 8
  and dim_queue = [0; 1; 2; 3; 4; 5; 6; 7] in

  (* Using a dimension only once, prevents the possibility of Queens being
   * placed in direct view, which halves the space we'll need to search, leaving
   * only the possibility of diagonals. *)

  let rec place_queens = function
    | 0, _, _ -> ()
    | num_queens, xq, yq ->
      let x = List.nth xq (Random.int (List.length xq))
      and y = List.nth yq (Random.int (List.length yq)) in

      board.(y).(x) <- Queen;

      let new_xq = List.filter ((!=) x) xq
      and new_yq = List.filter ((!=) y) yq in

      place_queens ((num_queens - 1), new_xq, new_yq)
  in
  place_queens (num_queens, dim_queue, dim_queue);
  board


let new_chromosome () =
  let gene_pool = [0; 1; 2; 3; 4; 5; 6; 7] in
  let chromosome = Array.make 8 0 in
  let rec shuffle = function
    | [], _ -> ()
    | gene_pool, position ->
      let pool_size = List.length gene_pool in
      let selected_gene = List.nth gene_pool (Random.int pool_size) in
      let new_gene_pool = List.filter ((!=) selected_gene) gene_pool in
      chromosome.(position) <- selected_gene;
      shuffle (new_gene_pool, position + 1)
  in
  shuffle (gene_pool, 0);
  chromosome


let board_of_chromosome chromosome =
  Array.map
  (fun x -> let row = Array.make 8 Empty in row.(x) <- Queen; row)
  chromosome


let main () =
  Random.self_init ();
  let population_size = 10 in
  let chromosomes = List.map (new_chromosome) (rep () population_size) in

  List.iter
  begin
    fun chromosome ->
      let board = board_of_chromosome chromosome in
      print_board board;
      print_newline ();
      print_board_weighted board;
      print_endline "===============";
      printf "BOARD WEIGHT: %d" (board_weight board);
      print_newline ();
      print_newline ()
  end
  chromosomes


let () = main ()
