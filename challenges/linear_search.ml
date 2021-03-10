let rec search n = function
    | [] -> false
    | x::xs -> if x = n then true else search n xs
