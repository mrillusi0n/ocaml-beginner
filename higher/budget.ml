let rec re_remaining_budget budget = function
    | [] -> budget
    | x::xs -> re_remaining_budget (budget-x) xs

let fr_remaining_budget budget lst =
    let (-) a b = b - a in
    List.fold_right (-) lst budget

let fl_remaining_budget budget =
    List.fold_left (-) budget

let b = 1024
let e = [1000; 20]
