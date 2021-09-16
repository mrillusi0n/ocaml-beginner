let compress list =
  let rec aux acc = function
    | [] -> acc
    | [x] -> x::acc
    | x :: (y::_ as t) -> if x = y then aux acc t else aux (x::acc) t
  in aux [] list |> List.rev
;;
