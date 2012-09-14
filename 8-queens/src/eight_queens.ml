open Printf
open Batteries


type square_state =
  Queen | Empty


type direction =
  North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


(* A permutation of integers 0 through 7. An integer's position represents row
 * (positin on y axis), integer value represents column (position on x axis).
 * This representation ensures that a dimension is used only once, thus
 * preventing a possibility of Queens being placed in direct view, which halves
 * the search space, leaving only the possibility of diagonals.
 *)
let new_chromosome () =
  Random.shuffle (0 -- 7)


let difference (a : 'a array) (b : 'a array) : 'a array =
  Array.filter (fun element -> not (Array.mem element b)) a


let is_probable = function
  | probability when (Random.float 1.0) <= probability -> true
  | _ -> false


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
  begin
    fun row ->
      Array.iter
      (print_square)
      row;
      print_newline ()
  end
  board;
  print_newline ()


let vector_fwd  = [ 1;  2;  3;  4;  5;  6;  7]
let vector_flat = [ 0;  0;  0;  0;  0;  0;  0]
let vector_rev  = [-1; -2; -3; -4; -5; -6; -7]


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
  offsets_of_dir direction
  |> List.map (fun (xo, yo) -> x + xo, y + yo)
  |> List.filter (is_onside)


let print_board_diagnostics board directions =
  Array.iteri
  (
    fun yi row ->
      Array.iteri
      (
        fun xi square -> match square with
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
              List.iter (fun (x, y) -> printf "%d,%d " x y) offsets;
              print_newline ();

              printf "%s-C : " (str_of_dir dir);
              List.iter (fun (x, y) -> printf "%d,%d " x y) view;
              print_newline ();

              printf "%s-S : " (str_of_dir dir);
              List.iter
              (fun (x, y) -> printf " %c  " (char_of_state board.(y).(x)))
              view;
              print_newline ();

              printf "%s-W : " (str_of_dir dir);
              List.iter
              (fun (x, y) -> printf " %d  " (weight_of_state board.(y).(x)))
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
  let views = List.map (view_in_dir x y) directions_diagonal in
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
  begin
    fun y row ->
      Array.iteri
      begin
        fun x state -> match state with
        | Queen -> printf "%d " (weight_of_position board x y)
        | Empty -> print_string "- "
      end
      row;
      print_newline ()
  end
  board


let weight_of_board board =
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


let board_of_chromosome chromosome =
  Array.map
  (fun x -> let row = Array.make 8 Empty in row.(x) <- Queen; row)
  chromosome


let weight_of_chromosome chromosome =
  weight_of_board (board_of_chromosome chromosome)


let print_chromosomes chromosomes label =
  print_endline label;
  Array.iteri
  begin
    fun i c ->
      printf "%d, " i;
      Array.iter (print_int) c;
      printf ", %d" (weight_of_chromosome c);
      print_newline ()
  end
  chromosomes;
  print_newline ()


let mutated chromosome =
  let chromosome = Array.copy chromosome in
  let length = Array.length chromosome in
  let point_a = Random.int length in
  let point_b = Random.int length in
  let val_of_a = chromosome.(point_a) in
  let val_of_b = chromosome.(point_b) in
  chromosome.(point_a) <- val_of_b;
  chromosome.(point_b) <- val_of_a;
  chromosome


let maybe_mutate_chromosomes chromosomes =
  let mutation_rate = 0.8 in
  Array.map
  (fun c -> if is_probable mutation_rate then mutated c else c)
  chromosomes


let crossover = function
  | [| parent_a; parent_b |] ->
    let cross_point = Random.int 8 in

    let head_a = Array.sub parent_a 0 cross_point in
    let head_b = Array.sub parent_b 0 cross_point in

    let tail_a = difference parent_b head_a in
    let tail_b = difference parent_a head_b in

    let child_a = Array.concat [head_a; tail_a] in
    let child_b = Array.concat [head_b; tail_b] in

    maybe_mutate_chromosomes [|child_a; child_b|]

  | _ -> assert false


let sort_population population =
  Array.sort
  (fun a b -> compare (weight_of_chromosome a) (weight_of_chromosome b))
  population


let evolve population time_to_stop =
  sort_population population;

  let population_size = Array.length population in

  let max_solutions = 92 in
  let max_generations = 1000 in
  let num_parent_candidates = 5 in

  let rec evolve = function
    | population, solutions, generation
      when (Unix.time ()) >= time_to_stop ->
      solutions

    | population, solutions, generation
      when generation >= max_generations ->
      solutions

    | population, solutions, generation
      when Enum.count (Set.enum solutions) >= max_solutions ->
      solutions

    | population, solutions, generation ->
      let parent_candidates =
        Array.make num_parent_candidates ()
        |> Array.map (fun () -> Random.int population_size)
        |> Array.map (fun i  -> population.(i))
      in
      sort_population parent_candidates;
      let parents = Array.sub parent_candidates 0 2 in
      let children = crossover parents in

      (* Mix-in children and drop the fattest members *)
      let new_population = Array.concat [population; children] in
      sort_population new_population;
      let new_population = Array.sub new_population 0 population_size in

      let solutions =
        new_population
        |> Array.filter (fun c -> (weight_of_chromosome c) = 0)
        |> Array.to_list
        |> Set.of_list
        |> Set.union solutions
      in
      evolve (new_population, solutions, generation + 1)
    in
    evolve (population, Set.empty, 0)


let main () =
  Random.self_init ();

  let population_size = 100 in
  let max_running_time = 5.0 in

  let time_started = Unix.time () in
  let time_to_stop = time_started +. max_running_time in

  let population =
    Enum.repeat ~times:population_size ()
    |> Enum.map (new_chromosome)
    |> Array.of_enum
  in

  evolve population time_to_stop
  |> Set.enum
  |> Enum.map (board_of_chromosome)
  |> Enum.iter (print_board)


let () = main ()
