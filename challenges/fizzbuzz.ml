let fizzbuzz vocable_pairs n =
	let rec aux acc = function
		| [] -> acc
		| (d, v) :: rest -> aux (if n % d = 0 then v :: acc else acc) rest 
	in
	match aux [] vocable_pairs with
	| [] -> Int.to_string n
	| l -> String.concat l

let (--) init term =
	List.init (term - init) (( + ) init)


let fizzbuzzer = List.map ~f:(fizzbuzz [
	(7,"Bazz");
	(5,"Buzz");
	(3,"Fizz");
])

let test = fizzbuzzer (1 -- 64)
