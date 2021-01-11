let rec fib_slow n =
    if n <= 1 then n
    else fib_slow (n-1) + fib_slow (n-2)

let rec h n pp p =
    if n = 1 then p
    else h (n-1) p (pp+p)

(** [fib n] is the nth Fibonnacci Number.
    Requires [n > 0] *)
let fib n = h n 0 1
