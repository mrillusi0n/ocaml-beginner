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

let dec_to_hex n =
  let rec aux acc x =
    if x = 0 then acc
    else aux (x % 16 :: acc) (x / 16)
  in
  (if n = 0 then [0] else aux [] n)
  |> List.map ~f:(Array.get hex_table)
  |> String.concat
;;

let hex_twenty = dec_to_hex 20 ;;
