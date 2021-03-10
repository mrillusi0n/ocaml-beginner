let magic x y : 'a option = Some 7 ;;

let rec merge_once times_merged acc = function
    | []            -> acc,times_merged
    | x::[]         -> x::acc,times_merged
    | x::xs::xss    -> match magic x xs with
        | None      -> merge_once times_merged (x::acc) (xs::xss)
        | Some a    -> merge_once (times_merged+1) (a::acc) xss
;;

let rec magic_merge merge_count lst =
    if merge_count = 0 then lst
    else let (merged,count) = merge_once 0 [] lst in
        magic_merge count merged
;;

let magic_wrapper = magic_merge 1
;;

