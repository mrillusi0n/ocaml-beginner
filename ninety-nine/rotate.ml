let sign = function
  | p when p > 0 -> `Positive
  | n when n < 0 -> `Negative
  | _ -> `Zero 
;;

let (%) x y =
  match sign x with
  | `Positive -> x mod y
  | `Negative -> y - (-x mod y)
  | `Zero -> 0
;;

let rotate list n =
  let rec split acc list n = (* [n] < length of the list *)
    match list, n with
    | x::xs, i when i > 0 -> split (x::acc) xs (n-1)
    | _ -> List.rev acc, list
  in
  let (left, right) = split [] list (n % (List.length list)) in
  List.rev_append (List.rev right) left
;;
