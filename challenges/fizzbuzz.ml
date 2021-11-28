let vocable_pairs = [
	(7,"Bazz");
	(5,"Buzz");
	(3,"Fizz");
]


let fizzbuzz n =
	let rec aux acc = function
		| [] -> acc
		| (d, v) :: rest -> aux (if n % d = 0 then v :: acc else acc) rest 
	in
	match aux [] vocable_pairs with
	| [] -> Int.to_string n
	| ls -> String.concat ls


let (--) init term =
	List.init (term - init) (( + ) init)


let fizzbuzzer = List.map ~f:fizzbuzz


let test = fizzbuzzer (1 -- 64)
