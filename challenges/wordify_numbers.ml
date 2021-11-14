open Base ;;

let named_numbers = [|
	"zero"
;	"one"
;	"two"
;	"three"
;	"four"
;	"five"
;	"six"
;	"seven"
;	"eight"
;	"nine"
;	"ten"
;	"eleven"
;	"twelve"
;	"thirteen"
;	"fourteen"
;	"fifteen"
;	"sixteen"
;	"seventeen"
;	"eighteen"
;	"nineteen"
|] ;;

let tens = [|
	"zero" (* will not be used *)
;	"ten"
;	"twenty"
;	"thirty"
;	"forty"
;	"fifty"
;	"sixty"
;	"seventy"
;	"eighty"
;	"ninety"
|] ;;

let power_name = [|
	None
;	Some "thousand"
;	Some "million"
;	Some "billion"
;	Some "trillion"
;	Some "quadrillion"
;	Some "quintillion"
|] ;;

let join_with_spaces = String.concat ~sep:" " ;;

let (<<) = Fn.compose ;;

let extract_digits number =
	let rec aux digits n = if n = 0 then digits else aux (n % 10 :: digits) (n / 10) in
	aux [] number
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
	|> extract_digits |> rev
	|> groupi ~break:(fun i _ _ -> i % 3 = 0)
	|> map ~f:(Option.map ~f:rev << group_to_words << rev)
	|> rev_filter_mapi ~f:name_group
	|> map ~f:(join_with_spaces << rev)
	|> join_with_spaces
;;

let tests = [
	(2000378   , "two million three hundred seventy eight");
	(1000      , "one thousand");
	(1040      , "one thousand forty");
	(31089     , "thirty one thousand eighty nine");
	(8000000   , "eight million");
	(0         , "zero");
	(178       , "one hundred seventy eight");
	(118       , "one hundred eighteen");
	(56        , "fifty six");
	(1         , "one");
] ;;

let () = List.iter tests (fun (test, expected) ->
	let status = match (String.equal expected (wordify_number test)) with
	| true -> "✓"
	| false -> "✗"
	in
	Stdio.print_endline (Printf.sprintf "%s %d" status test))
;;
