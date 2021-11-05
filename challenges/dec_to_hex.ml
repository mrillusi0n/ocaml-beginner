let hex_table = [|
  "0"
; "1"
; "2"
; "3"
; "4"
; "5"
; "6"
; "7"
; "8"
; "9"
; "A"
; "B"
; "C"
; "D"
; "E"
; "F"
|] ;;

let dec_to_bin n =
  let rec aux acc x =
    if x = 0 then acc
    else aux (x % 2 :: acc) (x / 2)
  in
  if n = 0 then [0] else aux [] n
;;

let bin_list_to_dec =
  Fn.compose snd (List.fold_left ~f:(fun (i, acc) bit -> (i+1, acc + (2 ** i) * bit)) ~init:(0, 0))
;;

let dec_to_hex n =
  let open List in
  n
  |> dec_to_bin
  |> rev
  |> groupi ~break:(fun i _ _ -> i % 4 = 0)
  |> map ~f:(Fn.compose (fun i -> hex_table.(i)) bin_list_to_dec)
  |> rev
  |> String.concat
;;

let hex_twenty = dec_to_hex 20 ;;
let bin_twenty = dec_to_bin 20 ;;
