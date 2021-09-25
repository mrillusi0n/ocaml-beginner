(* [padded_length] >= [String.length text] *)
let pad char padded_length text =
  let padding = String.make (padded_length - String.length text) char in
  String.concat [" "; text; padding; " "]

let make_separator widths =
  let pieces =
    widths
    |> List.map ~f:(fun len -> String.make (len + 2) '-')
    |> String.concat ~sep:"+"
  in
  String.concat ["|"; pieces; "|"]
;;

let make_row widths row =
  let padded =
    List.map2_exn widths row ~f:(pad ' ')
    |> String.concat ~sep:"|"
  in
  String.concat ["|"; padded; "|"]
;;

let max_widths header rows =
  let lengths = List.map ~f:String.length in
  let max_lengths res row = List.map2_exn res (lengths row) Int.max in
  List.fold_left rows ~init:(lengths header) ~f:max_lengths

let render_table header rows =
  let widths = max_widths header rows in
  let padded_widths = List.map widths (( + ) 2) in (* a space before and after *)
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
