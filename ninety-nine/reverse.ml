let rev list =
    let rec aux res = function
        | [] -> res
        | x::xs -> aux (x::res) xs
    in aux [] list
;;
