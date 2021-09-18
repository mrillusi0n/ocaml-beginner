let drop list n =
  let rec aux count acc = function
    | [] -> acc
    | x::xs ->
        let new_acc =
          if count mod n = 0 then acc
          else x::acc
        in aux (count+1) new_acc xs
  in List.rev (aux 1 [] list)
;;
