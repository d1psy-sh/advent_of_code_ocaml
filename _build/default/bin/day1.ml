let test_file = "test1.txt"

let input =
  In_channel.with_open_text test_file In_channel.input_all
  |> Str.(split (regexp "\n"))

let rec _print_lines input =
  match input with
  | [] -> ()
  | hd :: tl ->
      print_endline hd;
      _print_lines tl

let rec most_cals input res acc =
  match input with
  | [] -> res
  | hd :: tl -> (
      match hd with
      | "" ->
          most_cals tl (acc :: res) 0
      | some ->
          let cal = int_of_string some in
          most_cals tl res (acc + cal))

let rec max c m =
  match c with [] -> m | hd :: tl -> if hd > m then max tl hd else max tl m

let () =
  let cals = most_cals input [] 0 in
  max cals 0 |> string_of_int |> print_endline
