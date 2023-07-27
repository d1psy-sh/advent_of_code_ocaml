let file = "input4.txt"

let input =
  In_channel.with_open_text file In_channel.input_all
  |> Str.split (Str.regexp "\n")

let contains h =
  let parts = Str.split (Str.regexp ",") h in
  let first = Str.split (Str.regexp "-") (List.hd parts) in
  let second = Str.split (Str.regexp "-") (List.nth parts 1) in
  if
    int_of_string (List.nth first 1) <= int_of_string (List.nth second 1)
    && int_of_string (List.nth first 0) >= int_of_string (List.nth second 0)
    || int_of_string (List.nth first 1) >= int_of_string (List.nth second 1)
    && int_of_string (List.nth first 0) <= int_of_string (List.nth second 0)
  then true
  else false

let get_res input =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if contains h then aux (acc + 1) t else aux acc t
  in
  aux 0 input

let () = print_endline (string_of_int (get_res input))
