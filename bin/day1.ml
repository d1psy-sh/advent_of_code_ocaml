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
  | [] -> acc :: res
  | hd :: tl -> (
      match hd with
      | "" -> most_cals tl (acc :: res) 0
      | some ->
          let cal = int_of_string some in
          most_cals tl res (acc + cal))

let rec _max c m =
  match c with [] -> m | hd :: tl -> if hd > m then _max tl hd else _max tl m

let rec max3 c m1 m2 m3 =
  match c with
  | [] -> [ m1; m2; m3 ]
  | hd :: tl -> (
      match hd with
      | m1' when m1' > m1 -> max3 tl m1' m1 m2
      | m2' when m2' > m2 -> max3 tl m1 m2' m2
      | m3' when m3' > m3 -> max3 tl m1 m2 m3'
      | _ -> max3 tl m1 m2 m3)

let () =
  let cals = most_cals input [] 0 in
  let max_cals = max3 cals 0 0 0 in
  let rec sum list acc =
    match list with [] -> acc | hd :: tl -> sum tl (acc + hd)
  in
  let res = sum max_cals 0 in
  print_endline (string_of_int res)
