(* [padded_length] >= [String.length string] *)
let pad char padded_length string =
  let padding = String.make (padded_length - String.length string) char
  in String.concat [" "; string; padding; " "]

let make_separator widths =
  let pieces =
    List.map widths (fun len -> pad '-' len "") |> String.concat ~sep:"+"
  in String.concat ["|"; pieces; "|"]
;;

let make_row widths row =
  let padded =
    List.map2_exn widths row ~f:(pad ' ') |> String.concat ~sep:"|"
  in String.concat ["|"; padded; "|"]
;;

let max_widths header rows =
  let lengths = List.map ~f:String.length in
  List.fold_left
    rows ~init:(lengths header)
    ~f:(fun res row -> List.map2_exn res (lengths row) Int.max)

let render_table header rows =
  let widths = max_widths header rows in
  let padded_widths = List.map widths (( + ) 2) in (* a space *)
  (make_row padded_widths header)
  :: (make_separator padded_widths)
  :: (List.map rows (make_row padded_widths))
  |> String.concat ~sep:"\n"
;;

Stdio.print_endline
  (render_table
     ["language";"architect";"first release"]
     [ ["Lisp" ;"John McCarthy" ;"1958"] ;
       ["C"    ;"Dennis Ritchie";"1969"] ;
       ["ML"   ;"Robin Milner"  ;"1973"] ;
       ["OCaml";"Xavier Leroy"  ;"1996"] ;
])
;;

(**
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
**)
