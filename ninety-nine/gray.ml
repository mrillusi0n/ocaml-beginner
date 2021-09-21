let lpad n char str =
  let padding = n - String.length str in
  if padding < 0 then str else String.make (padding) char ^ str

let dec_to_bin n =
  let rec aux stack n =
    if n = 0 then stack
    else aux (n % 2 :: stack) (n lsr 1)
  in
  aux [] n
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:""
;;

let (<<) f g x = x |> g |> f

let gray n =
  let rec aux acc a =
    if a < 0 then acc else aux (a::acc) (a-1)
  in
  aux [] ((2 ** n) - 1) |> List.map ~f:(lpad n '0' << dec_to_bin)

