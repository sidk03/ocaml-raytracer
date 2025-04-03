type tuple = float*float*float*int 

let epsilon = 0.00001;;

let point (x:float) (y:float) (z:float) : tuple = (x,y,z,1)

let vector (x:float) (y:float) (z:float) : tuple = (x,y,z,0)

let is_point ((_,_,_,t):tuple) : bool = t = 1

let is_vector ((_,_,_,t):tuple) : bool = t = 0

let is_equal_tuple ((x1,y1,z1,t1):tuple) ((x2,y2,z2,t2):tuple) : bool = 
  abs_float(x1-.x2) < epsilon && abs_float(y1-.y2) < epsilon && abs_float(z1-.z2) < epsilon && t1=t2

let add ((x1,y1,z1,t1):tuple) ((x2,y2,z2,t2):tuple) : tuple = 
  if t1+t2 = 2 then failwith "Add 2 points" else (x1+.x2,y1+.y2,z1+.z2,t1+t2)

let sub ((x1,y1,z1,t1):tuple) ((x2,y2,z2,t2):tuple) : tuple = 
  if t1-t2 = -1 then failwith "Sub point from vector" else (x1-.x2,y1-.y2,z1-.z2,t1-t2)

  let neg ((x,y,z,t):tuple) : tuple = (0.-.x,0.-.y,0.-.z,t) (* can also define new unary op (~~)*)


let mult ((x,y,z,t):tuple) (m:float): tuple = (x*.m,y*.m,z*.m,t)

let div ((x,y,z,t):tuple) (d:float): tuple = (x/.d,y/.d,z/.d,t)

let magn ((x,y,z,t):tuple) : float = 
  if t = 1 then failwith "magnitude of point" else sqrt ((x*.x)+.(y*.y)+.(z*.z))

let norm ((x,y,z,t):tuple) : tuple = 
  if t = 1 then failwith "normalizing point" 
  else let m = magn (x,y,z,t) in div (x,y,z,t) m

let dot ((x1,y1,z1,t1):tuple) ((x2,y2,z2,t2):tuple) : float = 
  if t1 = 1 || t2 = 1 then failwith "dot of point" 
  else (x1*.x2)+.(y1*.y2)+.(z1*.z2)
  
let cross ((x1,y1,z1,t1):tuple) ((x2,y2,z2,t2):tuple) : tuple =
  if t1 = 1 || t2 = 1 then failwith "cross of point" 
  else vector (y1*.z2 -. z1*.y2) (z1*.x2 -. x1*.z2) (x1*.y2 -. y1*.x2)

type color = {r:float; g:float; b:float}

let make_color (r:float) (g:float) (b:float) : color = {r=r; g=g; b=b}

let add_color ({r=r1;g=g1;b=b1}:color) ({r=r2;g=g2;b=b2}:color) : color = {r = r1+.r2; g = g1+.g2; b = b1+.b2}

let sub_color ({r=r1;g=g1;b=b1}:color) ({r=r2;g=g2;b=b2}:color) : color = {r = r1-.r2; g = g1-.g2; b = b1-.b2}

let scalar_color ({r;g;b}:color) (s:float) : color = {r = r*.s; g = g*.s; b = b*.s}

let mult_color ({r=r1;g=g1;b=b1}:color) ({r=r2;g=g2;b=b2}:color) : color = {r = r1*.r2; g = g1*.g2; b = b1*.b2}

let is_equal_color ({r=r1;g=g1;b=b1}:color) ({r=r2;g=g2;b=b2}:color) : bool =
  abs_float(r1-.r2) < epsilon && abs_float(g1-.g2) < epsilon && abs_float(b1-.b2) < epsilon




  






