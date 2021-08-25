type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec mem x = function
    | Leaf -> false
    | Node (v,l,r) -> x = v || (x < v && mem v l) || mem v r
;;

let rec insert x  = function
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (v,l,r) as t ->
            if x = v then t
            else if x < v then Node (v, insert x l, r)
            else Node (v, l, insert x r)
;;
