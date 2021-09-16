let compress list =
  let rec aux acc = function
    | [] -> acc
    | [x] -> x::acc
    | x::y::xs -> if x = y then aux acc (y::xs) else aux (x::acc) (y::xs)
  in aux [] list |> List.rev
;;
