let fizzbuzz diags num =
  let rec say acc diags x =
    match diags with
    | [] -> (match acc with
        | [] -> None
        | _ -> Some acc)
    | (n,diag)::rest -> let new_acc =
      if x mod n = 0 then diag::acc
        else acc in
    say new_acc rest x
    in match say [] diags num with
    | None -> string_of_int num
    | Some x -> String.concat "" x
;;

let (--) init term =
  let rec aux acc curr =
    if curr < init then acc
    else aux (curr::acc) (curr-1)
  in aux [] term
;;


let fizzbuzzer = List.map (fizzbuzz [(5,"Buzz");(3,"Fizz")])
;;

print_string ((1--64) |> fizzbuzzer |> String.concat "\n")
;;

