let first_bigred = function
    | h::_ -> h = "bigred"
    | _ -> false

let l_two_four  = function
    | _::_::[] | _::_::_::_::[] -> true
    | _ -> false

let first_two_equal = function
    | first::second::_ -> first = second
    | _ -> false
