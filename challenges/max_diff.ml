let max_diff_inc nums =
  let aux num_diff_pair n =
    let (c, d) = num_diff_pair in
    let curr_diff = n - c in
    (Int.min c n, Int.max d curr_diff)
  in
  (match nums with
  | [] -> (-1, 0)
  | x :: xs -> List.fold_left xs ~init:(x, -1) ~f:aux)
  |> snd
;;

let test = [7; 1; 5; 4; 10]
;;

let res = max_diff_inc test
;;
