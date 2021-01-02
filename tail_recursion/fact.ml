let rec _fact n a =
    if n = 0 then a
    else _fact (n-1) (a*n)

let fact n = _fact n 1
