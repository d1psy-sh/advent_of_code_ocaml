let testString = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
let sol = 7
let file = "input5.txt"
let _input = In_channel.with_open_text file In_channel.input_all

module CharSet = Set.Make (Char)

let double_in_four one two three four =
  if
    one == two || one == three || one == four || two == three || two == four
    || three == four
  then true
  else false

let process input =
  let list = List.of_seq (String.to_seq input) in
  let rec aux acc list =
    match list with
    | [] -> acc
    | f :: (s :: t :: fourth :: _ as tl) ->
        if double_in_four f s t fourth then aux (acc + 1) tl else acc
    | _ -> acc
  in
  aux 4 list

let double_in_fourteen list =
  let set = CharSet.empty in
  let set = List.fold_right CharSet.add list set in
  if List.length (CharSet.elements set) == 14 then false else true

let process2 input =
  let list = List.of_seq (String.to_seq input) in
  let rec aux acc list =
    match list with
    (* get the first 14 elements of the list *)
    | [] -> acc
    | f
      :: (one :: two :: three :: four :: five :: six :: seven :: eight :: nine
          :: ten :: eleven :: twelve :: thirteen :: _ as window) ->
        if
          double_in_fourteen
            ([ f ] @ [ one ] @ [ two ] @ [ three ] @ [ four ] @ [ five ]
           @ [ six ] @ [ seven ] @ [ eight ] @ [ nine ] @ [ ten ] @ [ eleven ]
           @ [ twelve ] @ [ thirteen ])
        then aux (acc + 1) window
        else acc
    | _ -> acc
  in
  aux 14 list

let () =
  print_endline (string_of_int (process testString));
  print_endline (string_of_int (process _input));
  print_endline (string_of_int (process2 _input));
  print_endline (string_of_int sol)
