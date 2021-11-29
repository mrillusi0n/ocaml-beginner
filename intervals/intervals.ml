type ordering =
| Equal
| Greater
| Less

module type Comparable = sig
  type t
  val compare : t -> t -> ordering
end

module Make_interval (Endpoint : Comparable) = struct
  type t =
    | Empty
    | Interval of Endpoint.t * Endpoint.t

  let create low high =
    if Endpoint.compare low high = Greater then Empty
    else Interval (low, high)

  let is_empty = function
    | Empty -> true
    | _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) ->
      match Endpoint.(compare x l, compare x h) with
      | (Less, Greater) -> false
      | _ -> true

  let intersect t u =
    let min a b = if Endpoint.compare a b = Less then a else b in
    let max a b = if Endpoint.compare a b = Greater then a else b in

    match (t, u) with
    | (Interval (a, b), Interval (x, y)) -> create (max a x) (min b y)
    | _ -> Empty
end
