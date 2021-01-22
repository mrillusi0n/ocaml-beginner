type 'a tree = 
| Leaf 
| Node of 'a * 'a tree * 'a tree

let unbalanced_tree = Node (2, Node (4, Node (6, Leaf, Leaf), Leaf), Leaf)
