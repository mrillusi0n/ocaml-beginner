let inc x = x + 1
let dec x = x - 1

let bool_box v =
    (v, v < 0)

let below_zero f =
    fun x ->
        let v = f x in
        bool_box v

let (>>=) (boxed : int * bool) (f : int -> int * bool) : int * bool =
    let (v, b) = boxed in
    let (w, b') = f v in
    (w, b' || b)

let make_take_pair (f : int -> int) : int * bool -> int * bool =
    fun (v, b) ->
        let (w, b') = below_zero f v in
        (w, b' || b)

let inc' = below_zero inc
let dec' = below_zero dec


