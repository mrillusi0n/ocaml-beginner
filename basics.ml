(* simple math *)

let double n = n * 2
;;

let square n = n * n
;;

let cube n = n ** 3.
;;

let sign n = if n = 0 then 0 else if n > 0 then 1 else -1
;;

let area_circle r = 3.14 *. r ** 2.
;;

let root_mean_square x y = ((x ** 2. +. y ** 2.) /. 2.) ** 0.5
;;

let divide (numerator:float) (denominator:float) = numerator /. denominator
;;


(* printing *)

let rec print_int_list = function 
    | [] -> () 
    | h::t -> print_int h; 
              print_newline ();
              print_int_list t
;;

let print_int_list' lst = 
    List.iter (fun x -> print_int x;print_newline ();) lst
;;
