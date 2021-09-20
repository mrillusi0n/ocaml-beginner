let range init term =
  let rec aux acc curr =
    if curr < init then acc
    else aux (curr::acc) (curr-1)
  in aux [] term
;;
