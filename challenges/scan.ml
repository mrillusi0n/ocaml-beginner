let scan l ~f ?(init = 0) =
  let rec aux v acc = function
    | [] -> acc
    | x :: xs -> let v' = f v x in
    aux v' (v' :: acc) xs
  in
  List.rev (aux init [] l)
;;

let nums = [1; 2; 3; 4; 5; 6]
;;

let test = scan nums (+) 0
;;
