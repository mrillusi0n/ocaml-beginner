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

let rec square (Cons (h, tf)) =
    Cons (h*h, fun () -> square (tf ()))

let rec sum (Cons (a, ta)) (Cons (b, tb)) =
    Cons (a+b, fun () -> sum (ta ()) (tb ()))

let rec map f (Cons (h, tf)) =
    Cons (f h, fun () -> map f (tf ()))

let rec nats = Cons (1, fun () -> map (fun x -> x+1) nats)
