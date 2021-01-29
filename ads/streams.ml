type 'a stream =
    Cons of 'a * (unit -> 'a stream)

let rec from n =
    Cons (n, fun () -> from (n+1))

let hd (Cons (x,_)) = x
let tl (Cons (_,x)) = x ()

let rec take n s =
    if n = 0 then []
    else hd s :: take (n-1) (tl s)

let rec drop n s =
    if n = 0 then s
    else drop (n-1) (tl s)

let nats = from 0
