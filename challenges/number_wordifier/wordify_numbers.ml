let named_numbers =
[|  "zero"
  ; "one"
  ; "two"
  ; "three"
  ; "four"
  ; "five"
  ; "six"
  ; "seven"
  ; "eight"
  ; "nine"
  ; "ten"
  ; "eleven"
  ; "twelve"
  ; "thirteen"
  ; "fourteen"
  ; "fifteen"
  ; "sixteen"
  ; "seventeen"
  ; "eighteen"
  ; "nineteen" |]
;;

let tens =
[|  "one"
  ; "ten"
  ; "twenty"
  ; "thirty"
  ; "forty"
  ; "fifty"
  ; "sixty"
  ; "seventy"
  ; "eighty"
  ; "ninety" |]
;;

let power_name =
[|  "thousand"
  ; "million"
  ; "billion" |]
;;

let extract_digits number =
  let rec aux digits n =
    if n = 0 then digits else aux (n % 10 :: digits) (n / 10)
  in
  aux [] number
;;

let group_three list =
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

let join_with_spaces = String.concat ~sep:" "
;;

let pair_digit_with_place i digit = (digit, (10 ** (i % 3)))
;;

let pair_to_word = function
  | (0, _) -> None
  | (face, 1) -> Some named_numbers.(face)
  | (face, 10) -> Some tens.(face)
  | (face, _) -> Some (named_numbers.(face) ^ " hundred")
;;

let group_to_words group =
  let rec aux acc = function
    | [] -> acc
    | [(1, _); (x, _)] -> Some (named_numbers.(x+10)) :: acc
    | pair :: rest -> aux (pair_to_word pair :: acc) rest
  in
  aux [] group
;;

let wordify_number = function
  | 0 -> "zero"
  | n -> List.(match (n
  |> extract_digits
  |> rev
  |> rev_mapi ~f:pair_digit_with_place
  |> group_three
  |> map ~f:group_to_words
  |> rev_map ~f:(rev_filter_map ~f:Fn.id)
  |> map ~f:join_with_spaces
  |> map ~f:(fun s -> if String.is_empty s then None else Some s))
  with
  | [] -> "zero"
  | ones :: rest -> ones :: (rest
  |> mapi ~f:(fun i words -> Option.map words (fun words ->
      (words ^ " " ^ power_name.(i)))))
  |> rev_filter_map ~f:Fn.id
  |> join_with_spaces)
;;

let tests = [
  (2000378   , "two million three hundred seventy eight");
  (1000      , "one thousand");
  (1040      , "one thousand forty");
  (10400     , "ten thousand four hundred");
  (8000000   , "eight million");
  (0         , "zero");
  (178       , "one hundred seventy eight");
  (118       , "one hundred eighteen");
  (56        , "fifty six");
  (1         , "one");
]
;;

let () = List.iter tests (fun (test, expected) ->
  let message = 
    if (String.equal expected (wordify_number test))
    then "passed" else "failed"
  in
  Stdio.print_endline (Printf.sprintf "%d\t\t%s" test message)
)
;;
