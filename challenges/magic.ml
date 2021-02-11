let (<-<) f g x = x |> g |> f

let magik =
    List.map ((Stdlib.( * ) 2) <-< (Stdlib.( + ) 1))
    <-< List.filter ((((<>) 0) <-< (Stdlib.(land) 1)))
    <-< List.map abs
    <-< List.map int_of_float
    <-< List.map ((Fun.flip Stdlib.( ** )) 2.)
    <-< List.map float_of_int
    <-< List.filter ((>) 20)

let magic lst =
    lst
    |> List.filter ((>) 20)
    |> List.map float_of_int
    |> List.map ((Fun.flip Stdlib.( ** )) 2.)
    |> List.map int_of_float
    |> List.map abs
    |> List.filter ((((<>) 0) <-< (Stdlib.(land) 1)))
    |> List.map ((Stdlib.( * ) 2) <-< (Stdlib.( + ) 1))
