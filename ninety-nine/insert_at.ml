let rec split_take acc n = function
  | [] -> acc, []
  | (x::xs as l) ->
    if n = 0 then acc, l
    else split_take (x::acc) (n-1) xs
;;

let insert_at elem i list =
  let left, right = split_take [] i list in
  List.rev_append left (elem::right)
;;
