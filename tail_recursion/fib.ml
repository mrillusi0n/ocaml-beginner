let rec _fib n prev curr =
    if n = 1 then curr
    else _fib (n-1) (curr) (prev+curr)

let fib n = _fib n 0 1

