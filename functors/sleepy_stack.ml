module type StackSig = sig
    type 'a t

    val empty   : 'a t
    val push    : 'a -> 'a t -> 'a t
    val pop     : 'a t -> 'a t
    val peek    : 'a t -> 'a
end

module ListStack : StackSig = struct
    type 'a t = 'a list

    let empty       = []
    let push x s    = x::s
    let pop         = function [] -> failwith "Underflow" | _::t -> t
    let peek        = function [] -> failwith "Underflow" | x::_ -> x
end

module VarStack : StackSig = struct
    type 'a t = Empty | Entry of 'a * 'a t

    let empty       = Empty
    let push x s    = Entry (x,s)
    let pop         = function Empty -> failwith "Underflow" | Entry (_,t) -> t
    let peek        = function Empty -> failwith "Underflow" | Entry (x,_) -> x
end
