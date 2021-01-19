let sq_five = [1; 2; 3; 4; 5]
let co_five = 1::2::3::4::5::[]
let at_five = [1] @ [2; 3; 4] @ [5]

let rec prod = function
    | [] -> 1
    | h::t -> h * prod t

let rec raw_join = function
    | [] -> ""
    | h::t -> h ^ raw_join t
