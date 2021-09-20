let xrange ?step:(s=1) ?left:(l=0) right =
  let rec aux acc l =
    if l > right then acc
    else aux (l::acc) (l+s)
  in List.rev (aux [] l)

let range init term =
  if init < term then xrange ~left:init term
  else List.rev (xrange ~left:term init)
;;
