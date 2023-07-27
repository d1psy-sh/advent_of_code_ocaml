let file = "input3.txt"

let input =
  In_channel.with_open_text file In_channel.input_all
  |> Str.split (Str.regexp "\n")

let sum list =
  let rec aux acc = function [] -> acc | h :: t -> aux (acc + h) t in
  aux 0 list

let get_double string =
  let len = String.length string in
  let half = len / 2 in
  let first = String.sub string 0 half in
  let second = String.sub string half half in
  let first_char_array = List.of_seq (String.to_seq first) in
  let rec for_all fi se =
    match fi with
    | [] -> '0'
    | c :: t -> if String.contains se c then c else for_all t se
  in
  for_all first_char_array second

let get_common one tow three =
  let array_one = List.of_seq (String.to_seq one) in
  let rec aux list strtwo strthree =
    match list with
    | [] -> '0'
    | c :: t ->
        if String.contains strtwo c && String.contains strthree c then c
        else aux t strtwo strthree
  in
  aux array_one tow three

let get_prio char =
  match char with
  | 'a' .. 'z' ->
      let res = int_of_char char - int_of_char 'a' + 1 in
      res
  | 'A' .. 'Z' ->
      let res = int_of_char char - int_of_char 'A' + 27 in
      res
  | _ -> -1

let get_res input =
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        let double = get_double h in
        let prio = get_prio double in
        aux (prio :: acc) t
  in
  aux [] input

let get_res2 input =
  let rec aux acc = function
    | [] -> acc
    | one:: two:: three :: tail ->
        let common = get_common one two three in
        let prio = get_prio common in
        aux (prio :: acc) tail
    | _ -> acc
  in
  aux [] input

let () =
  print_endline (string_of_int (sum (get_res input)));
  print_endline (string_of_int (sum (get_res2 input)))
