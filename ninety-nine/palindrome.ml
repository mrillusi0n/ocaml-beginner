let is_palindrome list =
    let rec aux = function
        | x::xs, y::ys -> if x = y then aux (xs, ys) else false
        | _ -> true
    in (list, rev list) |> aux
;;
