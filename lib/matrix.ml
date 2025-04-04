type matrix = float array array

let epsilon = 0.00001;;

let empty_matrix (row:int) (col:int) : matrix = Array.make_matrix row col 0.

let set_index (m:matrix) (row:int) (col:int) (v:float) : unit = m.(row).(col) <- v

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

let det_2x2 (m:matrix) : float = 
  if matrix_size m <> (2,2) then failwith "Not a 2x2 Matrix" 
  else (m.(0).(0) *. m.(1).(1)) -. (m.(0).(1) *. m.(1).(0))
   
let remove_row (m:matrix) (row_no:int) : matrix  =
  let front = Array.sub m 0 row_no in 
  let back = Array.sub m (row_no+1) ((Array.length m)-(row_no+1)) in
  Array.append front back

(* 0 indexed !!!*)
let submatrix (m:matrix) (row:int) (col:int) : matrix = 
  let rem_row = remove_row m row in 
  let t_matrix = transpose rem_row in 
  transpose (remove_row t_matrix col) 

let rec det (m:matrix) : float =
  let (r,c) = matrix_size m in if (r,c) = (1,1) then m.(0).(0) 
  else if (r,c) = (2,2) then det_2x2 m 
  else let row_cof = Array.mapi (fun idx el -> (cofactor m idx 0) *. el.(0)) m in
  Array.fold_left (+.) 0. row_cof
  
and minor (m:matrix) (row:int) (col:int) : float =
  let sub = submatrix m row col in det sub
  
and cofactor (m:matrix) (row:int) (col:int) : float =
  let min = minor m row col in 
  if (row+col) mod 2 = 0 then min else 0.-.min

let is_invertable (m:matrix) : bool = 
  if abs_float(det m -. 0.) < epsilon then false else true

let inverse (m:matrix) : matrix = 
  let d = det m in 
  let cof = Array.mapi(fun r_idx row -> Array.mapi (fun c_idx _ -> (cofactor m r_idx c_idx)/.d) row) m in 
  transpose cof