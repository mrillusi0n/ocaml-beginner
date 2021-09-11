type 'a link = End | Node of 'a * 'a link
;;

let rec drop l n =
    match n with
    | 0 -> l
    | _ -> let Node (h,t) = l
    in drop t (n-1)
;;

let kth_last_node l k =
    let rec aux proxy head_k = match head_k with
        | End -> let Node (h,_) = proxy in h
        | Node (h,t) -> let Node (ph, pt) = proxy
        in aux pt t
    in aux l (drop l (k+1))
;;

let nums =
    Node (1, Node (2, Node (3, Node (4, End))))
;;

let s = kth_last_node nums;;
let d = drop nums;;
