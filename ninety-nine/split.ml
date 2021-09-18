let rec split_take acc n = function
  | [] -> acc, []
  | (x::xs as l) ->
    if n = 0 then acc, l
    else split_take (x::acc) (n-1) xs
;;

let split list n =
  let (rev_left, right) = split_take [] n list in
  List.rev rev_left, right
;;

let strs = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
