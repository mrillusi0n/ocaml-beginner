type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | (One x) :: xs -> x :: flatten xs
  | (Many ys) :: zs -> flatten ys @ flatten zs
;;
