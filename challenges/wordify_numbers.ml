let digit_to_word = function
  | 0 -> "zero"
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

let eleven_to_twenty = function
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

let pair_place_face_values digits =
  digits
  |> List.rev
  |> List.rev_mapi ~f:(fun i digit -> (digit, (10 ** i)))
;;
