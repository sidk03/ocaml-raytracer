open Features

type canvas = color array array

let make_canvas (w:int) (h:int) : canvas = Array.make_matrix h w {r=0.;g=0.;b=0.}

let pixel_at (c:canvas) (row:int) (col:int) = c.(row).(col)

let write_pixel (c:canvas) (row:int) (col:int) (pixel:color) : unit = c.(row).(col) <- pixel

let height_c (c:canvas) : int = Array.length c

let width_c (c:canvas) : int = Array.length c.(0)

let out_ppm_header (c:canvas) (oc:out_channel): unit = Printf.fprintf oc "P3\n%d %d\n%d\n" (width_c c) (height_c c) 225

let convert_color_pixel ({r;g;b}:color) (low:int) (high:int) : int*int*int = 
  let r = int_of_float (r*.255.) in let g = int_of_float (g*.255.) in let b = int_of_float (b*.255.) in
  ((if r < low then low else if r > high then high else r),
   (if g < low then low else if g > high then high else g),
   (if b < low then low else if b > high then high else b))

let out_row (row: color array) (oc:out_channel) =
  Array.iter (fun color -> let (r,g,b) = convert_color_pixel color 0 255 in Printf.fprintf oc "%d %d %d " r g b) row

let out_canvas (c:canvas) (oc:out_channel) =  
  Array.iter (fun row -> out_row row oc; Printf.fprintf oc "\n") c

let canvas_to_ppm (c:canvas) (file:string) : unit =
  let oc = Out_channel.open_text file  in 
  out_ppm_header c oc;
  out_canvas c oc;
  Out_channel.close oc
