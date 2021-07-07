let rec _powerset s = function
    | [] -> s
    | x::xs -> let t =
        List.rev_map (List.cons x) s
    in _powerset (List.rev_append t s) xs


let powerset = _powerset [[]]
