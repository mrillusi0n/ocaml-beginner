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
  | 1000 -> "thousand"
  | 1000000 -> "million"
  | 1000000000 -> "billion"
  | _ -> ""
;;

let extract_digits number =
  let rec aux digits n =
    if n = 0 then digits else aux (n % 10 :: digits) (n / 10)
  in
  aux [] number
;;

let group list =
  let rec aux curr acc n = function
    | [] ->
        if List.is_empty curr then acc else curr::acc
    | x::xs ->
      let (curr', acc') =
        if n % 3 = 0 then ([], (x::curr)::acc) else (x::curr, acc)
      in
      aux curr' acc' (n+1) xs
  in
  List.rev list 
    |> aux [] [] 1
;;

let face_place_pair_to_word = function
  | (0, _) -> ""
  | (face, place) -> (digit_to_word face) ^ " " ^ (place_to_word place)
;;

let rec face_place_pairs_to_words acc = function
  | [] -> acc
  | (0, _) :: rest -> face_place_pairs_to_words acc rest
  | [(digit, _)] -> digit_to_word digit :: acc
  | [(tens, _); (0, _)] -> tenth_to_word tens :: acc
  | [(1, _); (ones, _)] -> eleven_to_twenty_to_word (10 + ones) :: acc
  | [(tens, _); (ones, _)] ->
      (digit_to_word ones) :: (tenth_to_word tens) :: acc
  | pair :: rest ->
      face_place_pairs_to_words (face_place_pair_to_word pair :: acc) rest
;;

let append_place_word place = function
  | [] -> None
  | l -> Some (if place = 0 then l else ((place_to_word (10 ** (3 * place))) :: l))
;;

let wordify_number = function
  | 0 -> "zero"
  | n -> n
    |> extract_digits
    |> List.rev
    |> List.rev_mapi ~f:(fun i digit -> (digit, (10 ** (i % 3))))
    |> group
    |> List.rev_map ~f:(face_place_pairs_to_words [])
    |> List.filter_mapi ~f:append_place_word
    |> List.rev_map ~f:List.rev
    |> List.map ~f:(String.concat ~sep:" ")
    |> String.concat ~sep:" "
;;

let tests = [123456; 1000; 1042; 65534; 300000; 5000000; 1000000000]
;;

let res = tests
    |> List.map ~f:wordify_number
    |> List.zip_exn tests
;;
