let rec split_take acc n = function
  | [] -> acc, []
  | (x::xs as l) ->
    if n = 0 then acc, l
    else split_take (x::acc) (n-1) xs
;;

let remove_at ~pos ~list =
  let (left, right) = split_take [] pos list in
  match right with
  | [] -> list
  | _::xs -> List.rev_append left xs
;;


