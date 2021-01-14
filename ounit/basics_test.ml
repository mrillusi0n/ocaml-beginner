open OUnit2
open Basics

let make_test name f expected input =
    name >:: (fun _ -> assert_equal expected (f input) ~printer:string_of_int)

let double_tests = "test suite for double" >::: [
  make_test "zero"  double 0 0;
  make_test "one"   double 2 1;
  make_test "four"  double 8 4;
]

let square_tests = "test suite for square" >::: [
  make_test "zero"  square 0  0;
  make_test "-one"  square 1 (-1);
  make_test "four"  square 16 4;
]

let _ = run_test_tt_main double_tests
let _ = run_test_tt_main square_tests
