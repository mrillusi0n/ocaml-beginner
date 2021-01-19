let get_fifth lst =
    if List.length lst < 5 then 0
    else List.nth lst 4

let sort_desc (lst : int list) =
    List.rev (List.sort Stdlib.compare lst)
