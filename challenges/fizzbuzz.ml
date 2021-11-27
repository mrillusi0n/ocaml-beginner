type word = Number of int | Vocable of string 

let string_of_word = function
	| Number n -> Int.to_string n
	| Vocable v -> v

let fizzbuzz vocable_pairs n =
	let rec aux acc = function
		| [] -> acc
		| (d, v) :: rest -> aux (if n % d = 0 then v :: acc else acc) rest 
	in
	let res = aux [] vocable_pairs in
	(if List.is_empty res then Number n else Vocable (String.concat res))
	|> string_of_word

let (--) init term =
	List.init (term - init) (( + ) init)


let fizzbuzzer = List.map ~f:(fizzbuzz [
	(7,"Bazz");
	(5,"Buzz");
	(3,"Fizz")])

let test = fizzbuzzer (1 -- 64)

