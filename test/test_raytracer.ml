open OUnit2
open Raytracer.Features

let epsilon = 0.00001;;
let tuple_check _ = 
  let p = point 4.3 (-4.2) 3.1 in 
  let v = vector 4.3 (-4.2) 3.1 in 
  assert_equal true (is_point p);
  assert_equal false (is_vector p);
  assert_equal true (is_vector v);
  assert_equal false (is_point v)

let add_tuple _ = 
  let p = point 3. (-2.) 5. in 
  let v = vector (-2.) 3. 1. in
  assert_equal true (is_equal_tuple (point 1. 1. 6.) (add p v))

let sub_tuple _ =
  let p1 = point 3. 2. 1. in 
  let p2 = point 5. 6. 7. in
  let v1 = vector 5. 6. 7. in
  let v2 = vector 3. 2. 1. in
  assert_equal true (is_equal_tuple (vector (-2.) (-4.) (-6.)) (sub p1 p2));
  assert_equal true (is_equal_tuple (point (-2.) (-4.) (-6.)) (sub p1 v1));
  assert_equal true (is_equal_tuple (vector (-2.) (-4.) (-6.)) (sub v2 v1));
  assert_raises (Failure "Sub point from vector") (fun () -> sub v1 p1)

let neg_tuple _ = 
  let v = vector 1. (-2.) 3. in 
  assert_equal true (is_equal_tuple (vector (-1.) 2. (-3.)) (neg v))

let mult_div_tuple _ =
  let p = point 1. (-2.) 3. in
  let v = vector 1. (-2.) 3. in
  assert_equal true (is_equal_tuple (point 3.5 (-7.) 10.5) (mult p 3.5));
  assert_equal true (is_equal_tuple (vector 3.5 (-7.) 10.5) (mult v 3.5));
  assert_equal true (is_equal_tuple (point 0.5 (-1.) 1.5) (div p 2.));
  assert_equal true (is_equal_tuple (vector 0.5 (-1.) 1.5) (div v 2.))

let mag_tuple _ =
  let v1 = vector 1. 0. 0. in 
  let v2 = vector 1. 2. 3. in
  let v3 = vector (-1.) (-2.) (-3.) in
  let p = point 1. 2. 3. in
  assert_equal true ((abs_float (magn v1)-.1.) < epsilon);
  assert_equal true ((abs_float ((magn v2)-.(sqrt 14.))) < epsilon);
  assert_equal true ((abs_float ((magn v3)-.(sqrt 14.))) < epsilon);
  assert_raises (Failure "magnitude of point") (fun () -> magn p)

let norm_tuple _ =
  let v1 = vector 4. 0. 0. in
  let v2 = vector 1. 2. 3. in
  let p = point 1. 2. 3. in
  assert_equal true (is_equal_tuple (vector 1. 0. 0.) (norm v1));
  assert_equal true (is_equal_tuple (div v2 (magn v2)) (norm v2));
  assert_raises (Failure "normalizing point") (fun () -> norm p)

let dot_tuple _ =
  let v1 = vector 1. 2. 3. in 
  let v2 = vector 2. 3. 4. in 
  let p = point 1. 2. 3. in
  assert_equal true ((abs_float((dot v1 v2) -. 20.)) < epsilon);
  assert_raises (Failure "dot of point") (fun () -> dot p v1)

let cross_tuple _ = 
  let v1 = vector 1. 2. 3. in 
  let v2 = vector 2. 3. 4. in
  let p = point 1. 2. 3. in
  assert_equal true (is_equal_tuple (vector (-1.) 2. (-1.)) (cross v1 v2));
  assert_equal true (is_equal_tuple (vector 1. (-2.) 1.) (cross v2 v1));
  assert_raises (Failure "cross of point") (fun () -> cross p v1)


let test_suite = "test suite for raytracer" >::: [
  "tuple_check" >:: tuple_check;
  "add_tuple" >:: add_tuple;
  "sub_tuple" >:: sub_tuple;
  "neg_tuple" >:: neg_tuple;
  "mult_div_tuple" >:: mult_div_tuple;
  "mag_tuple" >:: mag_tuple;
  "norm_tuple" >:: norm_tuple;
  "dot_tuple" >:: dot_tuple;
  "cross_tuple" >:: cross_tuple;
]

let _ = run_test_tt_main test_suite
  

