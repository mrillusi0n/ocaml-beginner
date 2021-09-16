let encode list =
  let rec aux run acc = function
    | [] -> acc
    | [x] -> (run+1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (run+1) acc t
        else aux 0 ((run+1, a) :: acc) t
  in List.rev (aux 0 [] list)
;;
        
