let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::x::acc) xs
  in List.rev (aux [] list)
;;
