let pad char len string =
  let str_len = String.length string in
  if str_len >= len then string
  else " " ^ string ^ (String.make (len - str_len) char) ^ " "

let make_separator column_lengths =
  "|" ^ (List.map column_lengths (fun len -> String.make (len + 2) '-')
  |> String.concat ~sep:"+") ^ "|"
;;

let make_row column_lengths data =
  "|" ^ (List.map2_exn column_lengths data ~f:(pad ' ')
  |> String.concat ~sep:"|") ^ "|"

let make_rows list column_lengths =
  list
  |> List.map ~f:(make_row column_lengths)

let render_table headers rows =
  let get_string_lenghts = List.map ~f:String.length in
  let tf_rows = rows |> List.map ~f:get_string_lenghts in
  let tf_headers = get_string_lenghts headers in
  let max_lengths =
    List.fold_left tf_rows ~init:tf_headers ~f:(List.map2_exn ~f:Int.max)
    |> List.map ~f:(( + ) 2)
  in
     (make_row max_lengths headers)
  :: (make_separator max_lengths)
  :: (make_rows rows max_lengths)
  |> String.concat ~sep:"\n"
;;

Stdio.print_endline
  (render_table
    ["language";"architect";"first release"]
    [ ["Lisp" ;"John McCarthy" ;"1958"] ;
      ["C"    ;"Dennis Ritchie";"1969"] ;
      ["ML"   ;"Robin Milner"  ;"1973"] ;
      ["OCaml";"Xavier Leroy"  ;"1996"] ; ])
;;

(**
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
**)
