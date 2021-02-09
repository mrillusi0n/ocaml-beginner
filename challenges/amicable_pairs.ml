let rec _divisors d acc n =
    if n = 0 then []
    else if d = n then d::acc
    else if n mod d = 0 then _divisors (d+1) (d::acc) n
    else _divisors (d+1) acc n

let divisors n = _divisors 1 [] n

let sum_div = function
    | [] -> 0
    | _::xs -> List.fold_left (+) 0 xs

let (--) a b =
    let rec from acc a b =
        if a > b then acc
        else from (b::acc) a (b-1)
    in from [] a b

let x = sum_div (divisors 12)

let amicable_pairs =
    1 -- 10000
    |> List.map (fun x -> let sd = sum_div (divisors x) in
        (x,sd,sum_div (divisors sd)))
    |> List.filter (fun (a,b,s) -> a=s && a<>b)
    |> List.map (fun (a,b,s) -> (a,b))

let sum_amicable = List.map (fun (x,y) -> x+y) amicable_pairs
