type matrix = float array array

let epsilon = 0.00001;;

let empty_matrix (row:int) (col:int) : matrix = Array.make_matrix row col 0.

let matrix_size (m:matrix) : (int*int) = Array.length m, Array.length m.(0)

let is_equal_matrix (m1:matrix) (m2:matrix) : bool =
  if (matrix_size m1) <> (matrix_size m2) then false 
  else 
    let equal_row r1 r2 = Array.for_all2 (fun e1 e2 -> abs_float(e1-.e2) < epsilon) r1 r2 in
    Array.for_all2 equal_row m1 m2

let matrix_to_list (m:matrix) : (float list list) = 
  Array.to_list (Array.map (Array.to_list) m)

let list_to_matrix (l:float list list) : matrix =
  Array.of_list (List.map (Array.of_list) l)

let transpose (m:matrix) : matrix =
  let rec aux m = 
    match m with
    | [] -> [] 
    | [] :: _ -> [] 
    | _ -> begin
        let heads = List.map (List.hd) m in
        let tails = List.map (List.tl) m in 
        heads :: aux tails 
      end
  in list_to_matrix (aux (matrix_to_list m))

let mult_arrays (a:float array) (b:float array) : float = 
  let m = Array.map2 (fun e1 e2 -> e1*.e2) a b in Array.fold_left (+.) 0. m

let mult_matrix (a:matrix) (b:matrix) : matrix = 
  let t = transpose b in 
  Array.map (fun r -> Array.map (mult_arrays r) t) a

let identity_matrix (i : int) : matrix =
  let m = empty_matrix i i in Array.mapi (fun idx row -> row.(idx) <- 1.; row) m 
   
  

