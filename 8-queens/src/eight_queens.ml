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


let array_diff (a : 'a array) (b : 'a array) : 'a array =
  a
  |> Array.to_list
  |> List.filter (fun element -> not (List.mem element (Array.to_list b)))
  |> Array.of_list


let is_probable = function
  | probability when (Random.float 1.0) <= probability -> true
  | _ -> false


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

    let tail_a = array_diff parent_b head_a in
    let tail_b = array_diff parent_a head_b in

    let child_a = Array.concat [head_a; tail_a] in
    let child_b = Array.concat [head_b; tail_b] in

    maybe_mutate_chromosomes [|child_a; child_b|]

  | _ -> assert false


let sort_population population =
  Array.sort
  (fun a b -> compare (weight_of_chromosome a) (weight_of_chromosome b))
  population


let rec evolve = function
  (* Assuming population comes-in sorted by weight, of course... *)
  | population when (weight_of_chromosome population.(0)) = 0 -> population.(0)
  | population ->
    let population_size = Array.length population in
    let num_parent_candidates = 5 in

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

    evolve new_population


let main () =
  Random.self_init ();

  let population_size = 10 in
  let population =
    Array.of_list (List.map (new_chromosome) (rep () population_size))
  in
  sort_population population;

  let first_solution = evolve population in
  let board = board_of_chromosome first_solution in

  print_board board


let () = main ()
