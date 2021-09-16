let (--) i j =
    let rec aux acc n =
        if n < i then acc else aux (n::acc) (n-1)
    in aux [] (j-1)
;;

let (<<) f g x = x |> g |> f
;;

let make_mod max =
    (-max + 1) -- max
    |> List.map ((( - ) max << abs))
;;
