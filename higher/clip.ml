let clip n = 
    if n < 0 then 0 
    else if n > 10 then 10
    else n

let clip_list lst = List.map clip lst

let rec clipper = function
    | [] -> []
    | h::t -> (clip h)::(clipper t)

let rec _clipper acc = function
    | []  -> acc
    | h::t -> _clipper ((clip h)::acc) t

let clip_tr lst = List.rev (_clipper [] lst)

let compose f g x = x |> g |> f

let rec clipcomp = function
    | [] -> []
    | h::t -> compose (List.cons (clip h)) clipcomp t

let nums = [-1; -2; 2; 10; 100; 0; 5]
