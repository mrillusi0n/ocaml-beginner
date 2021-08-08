let rec solve required_denoms denoms change =
    if change = 0 then Some required_denoms
    else match denoms with
    | [] -> None
    | denom::rest -> let rd,c = if denom <= change then 
        (denom::required_denoms,change-denom)
        else (required_denoms,change)
    in solve rd rest c
;;

let dispense denoms =
    solve [] (List.sort compare denoms |> List.rev)
;;
