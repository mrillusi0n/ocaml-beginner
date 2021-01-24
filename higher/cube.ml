let (-->) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
    in from i j []

let cube n = n*n*n
let is_odd n = (n mod 2) = 1

let sum_cube_odd n =
    List.fold_left (+) 0 (List.filter is_odd (List.map cube (0-->n)))

let piped_sum_cube_odd n =
    0-->n |> List.map cube |> List.filter is_odd |> List.fold_left (+) 0
