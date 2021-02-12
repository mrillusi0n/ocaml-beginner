let tribonacci signature n =
    let rec trib sign n =
        let a::b::c::_ = sign in
        match n with
        | 0 | 1 | 2 | 3 -> sign
        | _ -> trib ((a+b+c)::sign) (n-1) in
    let a::b::c::_ = signature in
    match n with
    | 0 -> []
    | 1 -> [a]
    | 2 -> [a; b]
    | 3 -> signature
    | _ -> List.rev (trib (List.rev signature) n)

let seq = tribonacci [1; 1; 1] 10
