type 'a rle =
	| One of 'a
	| Many of int * 'a;;

let encode list =
	let rec aux run acc = function
		| [] -> acc
		| [x] -> (if run = 0 then One x else Many (run+1, x)) :: acc
		| a :: (b :: _ as t) ->
			if a = b then aux (run+1) acc t
			else let r = if run = 0 then One a else Many (run+1, a)
			in aux 0 (r :: acc) t
	in List.rev (aux 0 [] list)
;;


let decode list =
  let rec cons e times acc =
    if times = 0 then acc
    else cons e (times-1) (e::acc)
  in 
	let rec aux acc = function
		| [] -> acc
		| x::xs -> (match x with
		| One a -> aux (a::acc) xs
		| Many (count, b) -> aux (cons b count acc) xs)
	in List.rev (aux [] list)
;;
