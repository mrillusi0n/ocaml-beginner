let rec last_two = function
    | [] | [_]-> None
    | [a; b] -> Some (a, b)
    | _::xs -> last_two xs
;;
