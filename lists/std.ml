let get_fifth lst =
    if List.length lst < 5 then 0
    else List.nth lst 4

let sort_desc (lst : int list) =
    List.rev (List.sort Stdlib.compare lst)

let rec last_item lst =
    if List.tl lst = [] then List.hd lst
    else last_item (List.tl lst)

let any_zeroes =
    List.exists (fun x -> x == 0)

let rec take n lst =
    match lst with
    | []   -> []
    | h::t -> if n = 0 then [] else h::(take (n-1) t)

let rec t_take n lst =
    match lst,n with
    | [],_   -> []
    | _,0    -> []
    | h::t,_ -> h::(t_take (n-1) t)

let rec _take_tl acc n lst =
    match lst,n with
    | [],_   -> acc
    | _,0    -> acc
    | h::t,_ -> (_take_tl (acc @ [h]) (n-1) t)

let tl_take n lst = _take_tl [] n lst

let rec drop n lst =
    match lst,n with
    | [],_   -> []
    | lst,0  -> lst
    | _::t,_ -> (drop (n-1) t)

