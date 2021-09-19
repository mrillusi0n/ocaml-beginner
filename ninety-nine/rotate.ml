

let rotate list n =
  let rec split acc list n = (* [n] < length of the list *)
    match list, n with
    | x::xs, i when i > 0 -> split (x::acc) xs (n-1)
    | _ -> List.rev acc, list
  in
  let (left, right) = split [] list n in
  List.rev_append (List.rev right) left
;;
