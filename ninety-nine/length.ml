let rec _length acc = function
    | [] -> acc
    | _::t -> length' (acc+1) t
;;

let length = length' 0
