let rec _transpose acc m =
    if List.mem [] m then acc
    else _transpose (List.map List.hd m::acc) (List.map List.tl m)

let transpose m = List.rev (_transpose [] m)
let explode s = List.init (String.length s) (String.get s)
let int_to_char c = Char.code c - 48

let solve test =
    test
    |> List.map explode
    |> List.map (List.map (fun x -> if x = ' ' then 0 else 1))
    |> transpose
    |> List.map (List.fold_left (+) 0)
    |> List.map string_of_int
    |> String.concat " "
;;

let line = (read_line ())

let rows = match String.split_on_char ' ' line with
    | [] -> 0
    | x::_ -> int_of_string x
;;

let rec read_all_input acc n =
    if n = 0 then acc
    else read_all_input (read_line () :: acc) (n-1)


let test = read_all_input [] rows

let () = print_string (solve test)
let () = print_newline ()
