let rec unzip = function
  | [] -> ([],[])
  | (x1,y1) :: t ->
      let (a,b) = unzip t in
      (x1::a,y1::b)
;;


let rec zip (l1,l2) =
  match (l1,l2) with
  | ([],_) | (_,[]) -> []
  | (h1::t1,h2::t2) -> (h1,h2) :: (zip (t1,t2))
;;

