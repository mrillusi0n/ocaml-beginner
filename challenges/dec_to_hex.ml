let table = [|
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

let dec_to_other_base base n =
  let rec aux acc x =
    if x = 0 then acc
    else aux (table.(x % base) :: acc) (x / base)
  in
  String.concat (if n = 0 then ["0"] else aux [] n)
;;

let hex_twenty = dec_to_other_base 16 20 ;;
