open OUnit2
open Basics

let double_tests = "test suite for double" >::: [
  "zero"  >:: (fun _ -> assert_equal 0 (double 0));
  "one"   >:: (fun _ -> assert_equal 2 (double 1));
  "four"  >:: (fun _ -> assert_equal 8 (double 4));
]

let square_tests = "test suite for square" >::: [
  "zero"  >:: (fun _ -> assert_equal 0 (square 0));
  "-one"   >:: (fun _ -> assert_equal 1 (square (-1)));
  "four"  >:: (fun _ -> assert_equal 16 (square 4));
]

let _ = run_test_tt_main double_tests
let _ = run_test_tt_main square_tests
