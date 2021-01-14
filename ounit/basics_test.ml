open OUnit2
open Basics

let double_tests = "test suite for double" >::: [
  "zero"  >:: (fun _ -> assert_equal 0 (double 0) ~printer:string_of_int);
  "one"   >:: (fun _ -> assert_equal 2 (double 1) ~printer:string_of_int);
  "four"  >:: (fun _ -> assert_equal 8 (double 4) ~printer:string_of_int);
]

let square_tests = "test suite for square" >::: [
  "zero"  >:: (fun _ -> assert_equal 0 (square 0) ~printer:string_of_int);
  "-one"  >:: (fun _ -> assert_equal 1 (square (-1)) ~printer:string_of_int);
  "four"  >:: (fun _ -> assert_equal 16 (square 4) ~printer:string_of_int);
]

let _ = run_test_tt_main double_tests
let _ = run_test_tt_main square_tests
