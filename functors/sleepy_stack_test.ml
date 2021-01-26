open Sleepy_stack

module StackTester (StackMod: StackSig) = struct
    open StackMod

    let "push_pop_empty" >:: (fun _ ->
        assert_equal empty (empty |> push 0 |> pop))
end
