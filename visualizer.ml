open Table
  
type vis_method = Scatter2d | Hist2d | BarGraph | LineGraph

let string_of_elt = function
  | Int x -> string_of_int x
  | String x -> x
  | Float x -> string_of_float x
  | Bool x -> if x then "True" else "False"
      
let calc_widths t =
  let titles = List.map (fun (n, _) -> n) t.colnames in
  let title_widths = List.map String.length titles in
  let cur_max = List.map (fun x -> ref x) title_widths in
  let rec go_thru_elt_widths = function
    | None -> ()
    | Some n -> let row =
		  List.map (fun x -> String.length (string_of_elt !x)) n.value
		in
	        List.iter2 (fun r x -> if x > !r then r := x) cur_max row;
		go_thru_elt_widths (n.next) in
  go_thru_elt_widths (get_first t);
  List.map (!) cur_max
  
let make_sep_row widths sep spc =
  List.fold_left
    (fun s width -> s ^ (String.make (width + 2) spc) ^ sep)
    sep widths
let make_content_row entries widths =
  let fit e w =
    if (String.length e) <= w then
      e ^ (String.make (w - (String.length e)) ' ')
    else
      String.sub e 0 w in (* TODO *)
  List.fold_left2 (fun s e w -> s ^ " " ^ (fit e w) ^ " |") "|" entries widths
  
let print_tabular t =
  let titles = List.map (fun (n, _) -> n) t.colnames in
  let widths = calc_widths t in
  let sep_row = make_sep_row widths "+" '-' in
  print_endline sep_row;
  print_endline (make_content_row titles widths);
  print_endline sep_row;

  let rec print_entries = function
    | None -> ()
    | Some n ->
       let entries = List.map (fun x -> string_of_elt !x) n.value in
       print_endline (make_content_row entries widths);
       print_endline sep_row;
       print_entries n.prev in
    
  print_entries (get_last t)
    
let canvas_width = 60
let canvas_height = 20
let padding = 0.1
let new_canvas w h = Array.make_matrix w h ' '

(* return LL and UR bounds as a tuple ((x1, y1), (x2, y2)) *)
let bounds xcol ycol =
  let x1 = List.fold_left min infinity xcol in
  let y1 = List.fold_left min infinity ycol in
  let x2 = List.fold_left max neg_infinity xcol in
  let y2 = List.fold_left max neg_infinity ycol in
  let xb = (x2 -. x1) *. padding in
  let yb = (y2 -. y1) *. padding in
  ((x1 -. xb, y1 -. yb), (x2 +. xb, y2 +. yb))

let draw canvas x y c = canvas.(x).(y) <- c
let draw_horiz_line canvas y =
  if y >= 0 && y < Array.length canvas.(0) then
    for x = 0 to (Array.length canvas) - 1 do
      draw canvas x y '-'
    done
let draw_vert_line canvas x =
  if x >= 0 && x < Array.length canvas then
    for y = 0 to (Array.length canvas.(x)) - 1 do
      draw canvas x y '|'
    done
  
let print_canvas c xlab ylab =
  if Array.length c > 0 then
    for y = (Array.length c.(0)) - 1 downto 0 do
      (Printf.printf "%11s "
	(if List.mem_assoc y ylab then (List.assoc y ylab)
	 else ""));
      for x = 0 to (Array.length c) - 1 do
	print_char c.(x).(y);
      done;
      print_newline ()
    done;
    let ctr = ref 0 in
    Printf.printf "%12s" "";
    for x = 0 to (Array.length c) - 1 do
      if List.mem_assoc !ctr xlab then (
	let s = (List.assoc !ctr xlab) in
	print_string s;
        ctr := !ctr + String.length s)
      else (
	print_char ' ';
        incr ctr;
      )
    done;
    print_newline ()

let interpolate x a1 a2 b1 b2 =
  ((x -. a1) /. (a2 -. a1) *. (b2 -. b1) +. b1)

(* precondition: t has 2 columns, each of type int or float *)    
let vis_scatter2d t =
(* TODO: actually get xcol, ycol *)
  let xcol = [-1.; 2.; 3.; 4.] in
  let ycol = [-1.; 4.; 9.; 16.] in
  let canvas = new_canvas canvas_width canvas_height in
  let ((x1, y1), (x2, y2)) = bounds xcol ycol in
  let w = float_of_int (canvas_width - 1) in
  let h = float_of_int (canvas_height - 1) in
  (* draw axes *)
  let origin_x = if x1 <= 0. && x2 >= 0. then 0. else x1 in
  let origin_y = if y1 <= 0. && y2 >= 0. then 0. else y1 in
  draw_horiz_line canvas (int_of_float (interpolate 0. y1 y2 0. h));
  draw_vert_line canvas (int_of_float (interpolate 0. x1 x2 0. w));
  draw_horiz_line canvas (int_of_float (interpolate origin_y y1 y2 0. h));
  draw_vert_line canvas (int_of_float (interpolate origin_x x1 x2 0. w));
  
  (* draw points *)
  List.iter2 (fun x y ->
    draw canvas (int_of_float (interpolate x x1 x2 0. w))
                (int_of_float (interpolate y y1 y2 0. h)) '*')
    xcol ycol;
  print_canvas canvas
    [(int_of_float (interpolate 0. x1 x2 0. w), "0.0");
     (0, string_of_float x1); (canvas_width-1, string_of_float x2)]
    [(int_of_float (interpolate 0. y1 y2 0. h), "0.0");
     (0, string_of_float y1); (canvas_height-1, string_of_float y2)]
  
(* visualize [t] [v]: visualize the table [t] graphically using *)
(* the specified method [v]. *)
(* Throws an exception if the method is not compatible with *)
(* the table contents. *)
let visualize t = function
  | Scatter2d -> vis_scatter2d t
  | _ -> failwith "TODO"

(* legal_vis_methods [t] returns the legal visualization methods for *)
(* the table [t] *)
let legal_vis_methods t =
  failwith "TODO"    
