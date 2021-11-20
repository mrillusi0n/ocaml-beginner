let return x = Some x;;
let (>>=) m f = match m with
| None -> None
| Some v -> f v
;;

let safe_div x y = if y = 0 then None else Some (Stdlib.( / ) x y);;

let wrap op x y = Some (op x y);;
let propagate_none f x y =
  x >>= fun a ->
  y >>= fun b ->
    f a b
;;

let ( / ) = propagate_none safe_div;;
let ( + ) = propagate_none (wrap Stdlib.( + ));;
let ( - ) = propagate_none (wrap Stdlib.( - ));;
let ( * ) = propagate_none (wrap Stdlib.( * ));;
