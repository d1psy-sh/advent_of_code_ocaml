type play = Rock | Paper | Scissors
type res = Lose | Win | Draw

let file = "input2.txt"

let input =
  In_channel.with_open_text file In_channel.input_all
  |> Str.split (Str.regexp "\n")

let rec _test_print input =
  match input with
  | [] -> ()
  | h :: t ->
      print_endline h;
      _test_print t

let get_play char =
  match char with
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Invalid input"

let match_res char =
  match char with
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "Invalid input"

let get_score play1 play2 =
  let get_add p = match p with Paper -> 2 | Rock -> 1 | Scissors -> 3 in
  let score p1 p2 =
    match (p1, p2) with
    | Paper, Rock | Rock, Scissors | Scissors, Paper -> 0 + get_add p2
    | Paper, Paper | Rock, Rock | Scissors, Scissors -> 3 + get_add p2
    | Rock, Paper | Scissors, Rock | Paper, Scissors -> 6 + get_add p2
  in
  score play1 play2

let get_score2 play res =
  let res_add r = match r with Lose -> 0 | Draw -> 3 | Win -> 6 in
  let score play res =
    match (play, res) with
    | Paper, Win | Scissors, Draw | Rock, Lose -> 3 + res_add res
    | Paper, Draw | Scissors, Lose | Rock, Win -> 2 + res_add res
    | Paper, Lose | Scissors, Win | Rock, Draw -> 1 + res_add res
  in
  score play res

let ulitmate_res input sol_type =
  let solve1 h =
    let plays = Str.split (Str.regexp " ") h in
    let play1 = get_play (List.nth plays 0) in
    let play2 = get_play (List.nth plays 1) in
    get_score play1 play2
  in
  let solve2 h =
    let plays = Str.split (Str.regexp " ") h in
    let play1 = get_play (List.nth plays 0) in
    let play2 = match_res (List.nth plays 1) in
    get_score2 play1 play2
  in
  let solve input sol =
    let rec res list acc =
      match list with [] -> acc | h :: t -> res t (acc + sol h)
    in
    res input 0
  in
  match sol_type with 2 -> solve input solve2 | 1 -> solve input solve1 | _ -> 0

let () =
  let result = ulitmate_res input 1 in
  let result2 = ulitmate_res input 2 in
  print_endline (string_of_int result);
  print_endline (string_of_int result2)
