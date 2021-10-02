open Base ;;

let named_numbers = [|
  "zero"
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
; "nineteen"
|] ;;

let tens = [|
  "zero" (* will not be used *)
; "ten"
; "twenty"
; "thirty"
; "forty"
; "fifty"
; "sixty"
; "seventy"
; "eighty"
; "ninety"
|] ;;

let power_name = [|
  None
; Some "thousand"
; Some "million"
; Some "billion"
; Some "trillion"
; Some "quadrillion"
; Some "quintillion"
|] ;;

let join_with_spaces = String.concat ~sep:" " ;;

let (<<) = Fn.compose ;;

let extract_digits number =
  let rec aux digits n = if n = 0 then digits else aux (n % 10 :: digits) (n / 10) in
  aux [] number
;;

let group_three list =
  let rec aux curr acc n = function
    | [] -> if List.is_empty curr then acc else curr :: acc
    | x :: xs -> let (curr', acc') = match n % 3 with
    | 0 -> ([], (x :: curr) :: acc)
    | _ -> (x :: curr, acc)
    in
    aux curr' acc' (n+1) xs in
  List.rev list |> aux [] [] 1 
;;

let rec group_to_words = function
  | [] -> None
  | 0 :: digits -> group_to_words digits
  | [digit] -> Some [named_numbers.(digit)]
  | [1; digit] -> Some [named_numbers.(digit+10)]
  | [tenth; 0] -> Some [tens.(tenth)]
  | [tenth; first] -> Some [tens.(tenth); named_numbers.(first)]
  | h :: (_::_ as digits) -> Some ((named_numbers.(h) ^ " hundred")
  :: (Option.value (group_to_words digits) ~default:[]))
;;

let name_group p l = match power_name.(p) with
  | None -> l
  | Some name -> Option.map l (List.cons name)
;;

let wordify_number = function
  | n when n < 20 -> named_numbers.(n)
  | n -> let open List in n
  |> extract_digits
  |> group_three
  |> rev_map ~f:(Option.map ~f:rev << group_to_words)
  |> rev_filter_mapi ~f:name_group
  |> map ~f:(join_with_spaces << rev)
  |> join_with_spaces
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

let () = List.iter tests ~f:(fun (test, expected) ->
  let message = match (String.equal expected (wordify_number test)) with
  | true -> "✓"
  | false -> "✗"
  in
  Stdio.print_endline (Printf.sprintf "%s %d" message test))
;;
