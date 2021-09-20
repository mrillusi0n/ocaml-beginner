let xrange ?(step=1) ?(left=0) right =
  let rec aux acc left =
    if left > right then acc
    else aux (left::acc) (left+step)
  in List.rev (aux [] left)

let range init term =
  if init < term then xrange ~left:init term
  else List.rev (xrange ~left:term init)
;;
