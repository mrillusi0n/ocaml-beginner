let range a b =
  let op = if a <= b then ( + ) else ( - ) in
  List.init ((abs (a - b)) + 1) (op a)
