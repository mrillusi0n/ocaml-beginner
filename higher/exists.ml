let rec exists_rec f = function
    | [] -> false
    | x::xs -> (f x) || exists_rec f xs

let exists_fold f lst =
    List.fold_left (||) false (List.map f lst)

let exists_lib f =
    List.exists f

let nums = [1; 42; 5]
let odds = [1; 43; 5]

let is_even n = (n mod 2) = 0
