type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | (One x) :: xs -> aux (x::acc) xs
    | (Many ys) :: zs -> aux (aux acc ys) zs
  in aux [] list |> List.rev
;;
