type 'a tree =
| Leaf
| Node of 'a * 'a tree * 'a tree
;;

let kraine = Node (1,
    Node (2,
        Node (4, Leaf, Leaf),
        Node (5, Leaf, Leaf)),
    Node (3, Leaf, Leaf))
;;

let rec invert = function
    | Leaf -> Leaf
    | Node (d,l,r) -> Node (d, invert r, invert l)
;;
