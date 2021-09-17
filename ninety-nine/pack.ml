let pack list =
  let rec aux curr acc = function
    | [] -> acc
    | [x] -> (x :: curr) :: acc
    | x :: (y :: _ as t) ->
        if x = y then aux (x :: curr) acc t
        else aux [] ((x::curr)::acc) t
  in List.rev (aux [] [] list)
;;
