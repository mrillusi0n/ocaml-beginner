(** Division operator has problems with 0.
    We'll make it return None when it's
    divided by 0. *)

let safe_div a b =
    if b = 0 then None
    else Some (Stdlib.( / ) a b)

(** This doesn't play well with other operations.
    2 + 10 / 2 will fail the type check as [+]
    between an [int] and an [int option] is
    *not defined*. *)

(* Let's redefine every op to take [int option]. *)

let ( + ) (a : int option) (b : int option) =
    match a, b with
    | Some a, Some b -> Some (Stdlib.( + ) a b)
    | _ -> None

let ( / ) (a : int option) (b : int option) =
    match a, b with
    | _, Some 0 -> None
    | Some a, Some b -> Some (Stdlib.( / ) a b)
    | _ -> None


let _unbox_if_some op a b =
    match a, b with
    | Some a, Some b -> Some (op a b)
    | _ -> None

let unbox_if_some op a b =
    match a, b with
    | Some a, Some b -> op a b
    | _ -> None

let box_output op a b =
    Some (op a b)

let ( + ) = unbox_if_some (box_output Stdlib.( + ))
let ( - ) = unbox_if_some (box_output Stdlib.( - ))
let ( * ) = unbox_if_some (box_output Stdlib.( * ))
let ( / ) = unbox_if_some safe_div


