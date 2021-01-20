let safe_hd = function
    | [] -> None
    | x::xs -> Some x

let rec safe_tl = function
    | [] -> None
    | x::[] -> Some x
    | _::xs -> safe_tl xs

