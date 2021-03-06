open Printf
open Batteries


module S = Set
module L = List
module H = Hashtbl
module E = Enum
module A = struct
  include Array

  let diff (a : 'a array) (b : 'a array) : 'a array =
    filter (fun e -> not (mem e b)) a
end


module Utils = struct
  let timestamp () =
    let open Unix in
    let tm = time () |> localtime in
    sprintf
    "%04d-%02d-%02d--%02d-%02d-%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon  + 1)
     tm.tm_mday
     tm.tm_hour
     tm.tm_min
     tm.tm_sec


  let path_of components =
    String.concat Filename.dir_sep components


  let ensure_path path =
    let perms = 0o700 in
    let rec make_dirs = function
      | [], _ -> ()
      | dir::dir_queue, dir_acc ->
        let dir_acc = dir_acc @ [dir] in
        begin
          try Unix.mkdir (path_of dir_acc) perms
          with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
        end;
        make_dirs (dir_queue, dir_acc)
    in
    let dir_queue =
      Filename.dirname path |> Str.split (Str.regexp Filename.dir_sep)
    in
    make_dirs (dir_queue, [])
end




type square_state =
  Queen | Empty


type direction =
  North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest


type board =
  square_state array array


type chromosome =
  int array


type weight =
  int


type individual =
  weight * chromosome


type population =
  individual array


type solutions =
  chromosome S.t


type options =
  { population_size       : int
  ; num_parent_candidates : int
  ; mutation_rate         : float
  ; max_generations       : int
  ; max_solutions         : int
  ; max_running_time      : float
  ; time_to_stop          : float
  }




let timestamp = Utils.timestamp ()


let weights = H.create 100


let csv_delim = "|"


let path_file__stats = Utils.path_of ["data"; timestamp; "stats.csv"]
let path_file__facts = Utils.path_of ["data"; timestamp; "facts.csv"]


let log_facts (o : options) : unit =
  Utils.ensure_path path_file__facts;
  let oc = open_out path_file__facts in
  let header_line =
    String.join csv_delim
    [ "PopulationSize"
    ; "NumParentCandidates"
    ; "MutationRate"
    ; "MaxGenerations"
    ; "MaxSolutions"
    ; "MaxRunningTime"
    ]
  in
  let data_line =
    String.join csv_delim
    [ dump o.population_size
    ; dump o.num_parent_candidates
    ; dump o.mutation_rate
    ; dump o.max_generations
    ; dump o.max_solutions
    ; dump o.max_running_time
    ]
  in
  output_string oc (header_line ^ "\n");
  output_string oc (data_line ^ "\n");
  close_out oc


let get_opts (argv : string array) =
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

  let options =
    { population_size       = !population_size
    ; num_parent_candidates = !num_parent_candidates
    ; mutation_rate         = !mutation_rate
    ; max_generations       = !max_generations
    ; max_solutions         = !max_solutions
    ; max_running_time      = !max_running_time
    ; time_to_stop          = ((Unix.time ()) +. !max_running_time)
    }
  in
  log_facts options;
  options


(* A permutation of integers 0 through 7. An integer's position represents row
 * (positin on y axis), integer value represents column (position on x axis).
 * This representation ensures that a dimension is used only once, thus
 * preventing a possibility of Queens being placed in direct view, which halves
 * the search space, leaving only the possibility of diagonals.
 *)
let new_chromosome () =
  Random.shuffle (0 -- 7)


let is_probable = function
  | probability when (Random.float 1.0) <= probability -> true
  | _ -> false


let directions_all : direction list  =
  [North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest]


let directions_diagonal : direction list =
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


let print_board (b : board) =
  A.iter (fun row -> A.iter print_square row; print_newline ()) b;
  print_newline ()


let vector_fwd  = [ 1;  2;  3;  4;  5;  6;  7]
let vector_flat = [ 0;  0;  0;  0;  0;  0;  0]
let vector_rev  = [-1; -2; -3; -4; -5; -6; -7]


let offsets_N  = L.combine vector_flat vector_rev
let offsets_NE = L.combine vector_fwd  vector_rev
let offsets_E  = L.combine vector_fwd  vector_flat
let offsets_SE = L.combine vector_fwd  vector_fwd
let offsets_S  = L.combine vector_flat vector_fwd
let offsets_SW = L.combine vector_rev  vector_fwd
let offsets_W  = L.combine vector_rev  vector_flat
let offsets_NW = L.combine vector_rev  vector_rev


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


let view_in_dir x y (d : direction) =
  offsets_of_dir d
  |> L.map (fun (xo, yo) -> x + xo, y + yo)
  |> L.filter (is_onside)


let print_board_diagnostics (board : board) (directions : direction list) =
  A.iteri
  (
    fun yi row ->
      A.iteri
      (
        fun xi square -> match square with
        | Queen ->
          printf "(X: %d, Y: %d, STATE: %c)\n" xi yi (char_of_state square);
          L.iter
          (
            fun dir ->
              let offsets = offsets_of_dir dir in
              let view = view_in_dir xi yi dir in
              let view_weights =
                L.map (fun (x, y) -> weight_of_state board.(y).(x)) view
              in

              printf "%s-O : " (str_of_dir dir);
              L.iter (fun (x, y) -> printf "%d,%d " x y) offsets;
              print_newline ();

              printf "%s-C : " (str_of_dir dir);
              L.iter (fun (x, y) -> printf "%d,%d " x y) view;
              print_newline ();

              printf "%s-S : " (str_of_dir dir);
              L.iter
              (fun (x, y) -> printf " %c  " (char_of_state board.(y).(x)))
              view;
              print_newline ();

              printf "%s-W : " (str_of_dir dir);
              L.iter
              (fun (x, y) -> printf " %d  " (weight_of_state board.(y).(x)))
              view;
              print_newline ();

              printf "%s-WT: " (str_of_dir dir);
              let view_weight = L.fold_left (+) 0 view_weights in
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


let weight_of_position (b : board) x y =
  let views = L.map (view_in_dir x y) directions_diagonal in
  let weights =
    L.map
    (
      L.fold_left
      (fun acc (xv, yv) -> (weight_of_state b.(yv).(xv)) + acc)
      0
    )
    views
  in
  L.fold_left (+) 0 weights


let print_board_weighted (b : board) =
  A.iteri
  begin
    fun y row ->
      A.iteri
      begin
        fun x state -> match state with
        | Queen -> printf "%d " (weight_of_position b x y)
        | Empty -> print_string "- "
      end
      row;
      print_newline ()
  end
  b


let weight_of_board (b : board) =
  let board_weighted =
    A.mapi
    (
      fun y row ->
        A.mapi
        (
          fun x state -> match state with
          | Queen -> weight_of_position b x y
          | Empty -> 0
        )
        row
    )
    b
  in
  let weights = A.map (A.fold_left (+) 0) board_weighted in
  A.fold_left (+) 0 weights


let board_of_chromosome (c : chromosome) =
  A.map (fun x -> let row = A.make 8 Empty in row.(x) <- Queen; row) c


let weight_of_chromosome (c : chromosome) =
  let weight =
    try
      H.find weights c
    with Not_found ->
      let weight = weight_of_board (board_of_chromosome c) in
      H.add weights c weight;
      weight
  in
  weight


let print_chromosomes (chromosomes : chromosome list) label =
  print_endline label;
  L.iteri
  begin
    fun i c ->
      printf "%d, " i;
      A.iter (print_int) c;
      printf ", %d" (weight_of_chromosome c);
      print_newline ()
  end
  chromosomes;
  print_newline ()


let individual_of_chromosome (c : chromosome) =
  (weight_of_chromosome c), c


let mutated (c : chromosome) =
  let c = A.copy c in
  let length = A.length c in
  let point_a = Random.int length in
  let point_b = Random.int length in
  let val_of_a = c.(point_a) in
  let val_of_b = c.(point_b) in
  c.(point_a) <- val_of_b;
  c.(point_b) <- val_of_a;
  c


let maybe_mutate (mutation_rate : float) (c : chromosome) =
  if is_probable mutation_rate then mutated c else c


let crossover (mutation_rate : float) (parents : individual array) =
  match parents with
  | [| _, parent_a; _, parent_b |] ->
    let cross_point = Random.int 8 in

    let head_a = A.sub parent_a 0 cross_point in
    let head_b = A.sub parent_b 0 cross_point in

    let tail_a = A.diff parent_b head_a in
    let tail_b = A.diff parent_a head_b in

    [| [head_a; tail_a]; [head_b; tail_b] |]
    |> A.map (A.concat)
    |> A.map (maybe_mutate mutation_rate)
    |> A.map (individual_of_chromosome)

  | _ -> assert false


let sort_population (p : population) =
  A.sort (fun ((a : weight), _) ((b : weight), _) -> compare a b) p


let weight_stats (p : population) =
  let weights = A.map (fst) p in

  let length = A.length weights in
  let total  = A.fold_left (+) 0 weights in

  let hi  = A.max weights in
  let lo  = A.min weights in
  let avg = (float_of_int total) /. (float_of_int length) in

  (hi, lo, avg)


let stats_log_init () =
  Utils.ensure_path path_file__stats;
  let oc = open_out path_file__stats in
  let header_line =
    String.join csv_delim
    [ "TimeStamp"
    ; "Generation"
    ; "WeightHighest"
    ; "WeightLowest"
    ; "WeightAverage"
    ; "UniqueSolutions"
    ]
  in
  output_string oc (header_line ^ "\n");
  oc


let stats_log_record oc generation (p : population) (s : solutions) =
  let timestamp = Unix.time () in
  let hi, lo, avg = weight_stats p in
  let unique_solutions = s |> S.enum |> E.count in
  let data_line =
    String.join csv_delim
    [ string_of_float timestamp
    ; string_of_int generation
    ; string_of_int hi
    ; string_of_int lo
    ; string_of_float avg
    ; string_of_int unique_solutions
    ]
  in
  output_string oc (data_line ^ "\n")


let evolve (p : population) (o : options) =
  let oc = stats_log_init () in

  let rec evolve = function
    | _, solutions, _
      when (Unix.time ()) >= o.time_to_stop ->
      close_out oc;
      solutions

    | _, solutions, generation
      when generation >= o.max_generations ->
      close_out oc;
      solutions

    | _, solutions, _
      when E.count (S.enum solutions) >= o.max_solutions ->
      close_out oc;
      solutions

    | p, solutions, generation ->
      stats_log_record oc generation p solutions;

      let parent_candidates =
        A.make o.num_parent_candidates ()
        |> A.map (fun () -> Random.int o.population_size)
        |> A.map (fun i  -> p.(i))
      in
      sort_population parent_candidates;
      let parents = A.sub parent_candidates 0 2 in
      let children = crossover o.mutation_rate parents in

      (* Mix-in children and drop the fattest members *)
      let new_population = A.concat [p; children] in
      sort_population new_population;
      let new_population = A.sub new_population 0 o.population_size in

      let new_solutions =
        new_population
        |> A.filter (fst |- (=) 0)
        |> A.map (snd)
        |> A.to_list
        |> S.of_list
        |> S.union solutions
      in
      evolve (new_population, new_solutions, generation + 1)
    in
    sort_population p;
    evolve (p, S.empty, 0)


let main argv =
  Random.self_init ();

  let options = get_opts argv in

  let population =
    E.repeat ~times:options.population_size ()
    |> E.map (new_chromosome |- individual_of_chromosome)
    |> A.of_enum
  in

  evolve population options |> S.iter (board_of_chromosome |- print_board)


let () = main Sys.argv
