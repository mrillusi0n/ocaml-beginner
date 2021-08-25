module type Stack = sig
    type 'a t

    val empty : 'a t

    val is_empty    : 'a t -> bool
    val push        : 'a -> 'a t -> 'a t
    val pop         : 'a t -> 'a t option
    val peek        : 'a t -> 'a option
end;;

module ListStack : Stack = struct
    type 'a t = 'a list

    let empty = []

    let is_empty s = (s = [])

    let push x s = x::s

    let pop = function
        | [] -> None
        | _::xs -> Some xs

    let peek = function
        | [] -> None
        | x::_ -> Some x
end;;
