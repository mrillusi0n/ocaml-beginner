type covid_status = Negative | Untested | Positive

let show_warning status =
  match status with
  | Negative -> print_endline "Safe";
  | Untested -> print_endline "Not sure";
  | Positive -> print_endline "Unsafe";

