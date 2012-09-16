open Printf
open Batteries


type options =
  { population_size       : int
  ; num_parent_candidates : int
  ; mutation_rate         : float
  ; max_generations       : int
  ; max_solutions         : int
  ; time_to_stop          : float
  }


type square_state =
  Queen | Empty


type direction =
  North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


type chromosome =
  int array


type population =
  int * chromosome array


let get_opts argv =
  let usage = ""

  and population_size       = ref 100
  and num_parent_candidates = ref 5
  and mutation_rate         = ref 0.8
  and max_generations       = ref 1000
  and max_solutions         = ref 92
  and max_running_time      = ref 5.0
  in

  let speclist =
    Arg.align
    [ ("--population-size",       Arg.Set_int population_size,       "")
    ; ("--num-parent-candidates", Arg.Set_int num_parent_candidates, "")
    ; ("--mutation-rate",         Arg.Set_float mutation_rate,       "")
    ; ("--max-generations",       Arg.Set_int max_generations,       "")
    ; ("--max-solutions",         Arg.Set_int max_solutions,         "")
    ; ("--max-running-time",      Arg.Set_float max_running_time,    "")
    ]
  in

  Arg.parse speclist (fun _ -> ()) usage;

  { population_size       = !population_size
  ; num_parent_candidates = !num_parent_candidates
  ; mutation_rate         = !mutation_rate
  ; max_generations       = !max_generations
  ; max_solutions         = !max_solutions
  ; time_to_stop          = ((Unix.time ()) +. !max_running_time)
  }


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


let popmember_of_chromosome c =
  (weight_of_chromosome c), c


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


let maybe_mutate mutation_rate chromosome =
  if is_probable mutation_rate then mutated chromosome else chromosome


let crossover mutation_rate parents = match parents with
  | [| _, parent_a; _, parent_b |] ->
    let cross_point = Random.int 8 in

    let head_a = Array.sub parent_a 0 cross_point in
    let head_b = Array.sub parent_b 0 cross_point in

    let tail_a = difference parent_b head_a in
    let tail_b = difference parent_a head_b in

    [| [head_a; tail_a]; [head_b; tail_b] |]
    |> Array.map (Array.concat)
    |> Array.map (maybe_mutate mutation_rate)
    |> Array.map (popmember_of_chromosome)

  | _ -> assert false


let sort_population population =
  Array.sort
  (fun (weight_a, _) (weight_b, _) -> compare weight_a weight_b)
  population


let evolve population opts =
  sort_population population;

  let rec evolve = function
    | _, solutions, _
      when (Unix.time ()) >= opts.time_to_stop ->
      solutions

    | _, solutions, generation
      when generation >= opts.max_generations ->
      solutions

    | _, solutions, _
      when Enum.count (Set.enum solutions) >= opts.max_solutions ->
      solutions

    | population, solutions, generation ->
      let parent_candidates =
        Array.make opts.num_parent_candidates ()
        |> Array.map (fun () -> Random.int opts.population_size)
        |> Array.map (fun i  -> population.(i))
      in
      sort_population parent_candidates;
      let parents = Array.sub parent_candidates 0 2 in
      let children = crossover opts.mutation_rate parents in

      (* Mix-in children and drop the fattest members *)
      let new_population = Array.concat [population; children] in
      sort_population new_population;
      let new_population = Array.sub new_population 0 opts.population_size in

      let solutions =
        new_population
        |> Array.filter (fst |- (=) 0)
        |> Array.map (snd)
        |> Array.to_list
        |> Set.of_list
        |> Set.union solutions
      in
      evolve (new_population, solutions, generation + 1)
    in
    evolve (population, Set.empty, 0)


let main argv =
  Random.self_init ();

  let options = get_opts argv in

  let population =
    Enum.repeat ~times:options.population_size ()
    |> Enum.map (new_chromosome)
    |> Array.of_enum
    |> Array.map (popmember_of_chromosome)
  in

  evolve population options
  |> Set.enum
  |> Enum.map (board_of_chromosome)
  |> Enum.iter (print_board)


let () = main Sys.argv
