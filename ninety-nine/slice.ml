let take list n =
  let rec aux acc n list =
    match n, list with
    | 0, _ | _, [] -> acc
    | _, x::xs -> aux (x::acc) (n-1) xs in
  List.rev (aux [] list n)
;;

let rec drop list n =
  match n, list with
  | 0, _ | _, [] -> list
  | _, x::xs -> drop xs (n-1)
;;

let slice list i j =
  drop list i |> take (j-1)
;;

let letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]
;;
