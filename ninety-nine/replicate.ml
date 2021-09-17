let replicate list times =
  let rec cons e times acc =
    if times = 0 then acc
    else cons e (times-1) (e::acc)
  in
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (cons x times acc) xs
  in List.rev (aux [] list)
;;
