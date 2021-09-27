let digit_to_word = function
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | _ -> ""
;;

let eleven_to_twenty_to_word = function
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | _ -> ""
;;

let tenth_to_word = function
  | 1 -> "ten"
  | 2 -> "twenty"
  | 3 -> "thirty"
  | 4 -> "forty"
  | 5 -> "fifty"
  | 6 -> "sixty"
  | 7 -> "seventy"
  | 8 -> "eighty"
  | 9 -> "ninety"
  | _ -> ""
;;

let place_to_word = function
  | 100 -> "hundred"
  | 1_000 -> "thousand"
  | 1_000_000 -> "million"
  | _ -> ""

let extract_digits number =
  let rec aux digits n =
    if n = 0 then digits else aux (n % 10 :: digits) (n / 10)
  in
  aux [] number
;;

let num_to_digit_words n =
  extract_digits n
  |> List.map ~f:digit_to_word
  |> String.concat ~sep:" "
;;

let pair_face_place_values digits =
  digits
  |> List.rev
  |> List.rev_mapi ~f:(fun i digit -> (digit, (10 ** i)))
;;

let face_place_pair_to_word = function
  | (0, _) -> ""
  | (face, place) -> (digit_to_word face) ^ " " ^ (place_to_word place)
;;

let rec face_place_pairs_to_words acc = function
  | [] -> acc
  | [(digit, _)] | [(0, _); (digit, _)] -> digit_to_word digit :: acc
  | [(tens, _); (0, _)] -> tenth_to_word tens :: acc
  | [(1, _); (ones, _)] -> eleven_to_twenty_to_word (10 + ones) :: acc
  | [(tens, _); (ones, _)] ->
      (tenth_to_word tens ^ " " ^ digit_to_word ones) :: acc
  | pair :: rest ->
      face_place_pairs_to_words (face_place_pair_to_word pair :: acc) rest
;;

let wordify_number = function
  | 0 -> "zero"
  | n -> n
    |> extract_digits
    |> pair_face_place_values
    |> face_place_pairs_to_words []
    |> List.filter ~f:(fun w -> not (String.equal "" w))
    |> List.rev
    |> String.concat ~sep:" "
;;
