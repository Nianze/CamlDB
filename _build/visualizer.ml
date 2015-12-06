let cmp = compare
open Table
open Data_processor
  
type vis_method = VisNone | Scatter2d | Hist2d | BarGraph | LineGraph

let string_of_elt = function
  | Int x -> string_of_int x
  | String x -> x
  | Float x -> string_of_float x
  | Bool x -> if x then "true" else "false"
      
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
      String.sub e 0 w in (* chop string if it doesn't fit in the cell *)
  List.fold_left2 (fun s e w -> s ^ " " ^ (fit e w) ^ " |")
      "|" entries widths
  
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
(* Source: Bresenham's line algorithm.    https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm *)      
let draw_line canvas x0 y0 x1 y1 =
  let (y0, y1) = if x0 < x1 then (y0, y1) else (y1, y0) in
  let (x0, x1) = (min x0 x1, max x0 x1) in
  if x0 = x1 then
    for y = y0 to y1 do
      draw canvas x0 y '*'
    done
  else
    let error = ref 0.0 in
    let deltaerr =
      ref (float_of_int (abs(y1 - y0)) /. (float_of_int (x1 - x0))) in
    let y = ref y0 in
    for x = x0 to x1 do
      draw canvas x !y '.';
      error := !error +. !deltaerr;
      while !error >= 0.5 do
	draw canvas x !y '.';
	y := !y + (int_of_float(copysign 1.0 (float_of_int(y1 - y0))));
	error := !error -. 1.0
      done
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

(* precondition: t has two columns, each of type Float or Int *)    
let get_cols t =
  let colnames = fst (List.split (get_colnames t)) in
  let fst_t = snd (select_col [List.nth colnames 0] t) in
  let snd_t = snd (select_col [List.nth colnames 1] t) in
  let to_num x =
    match !(List.hd x.value) with
    | Float x -> x
    | Int x -> float_of_int x
    | _ -> failwith "Visualizer: invalid data type" in
  List.split (List.sort cmp (List.combine
			       (List.map to_num (table_to_list fst_t))
                               (List.map to_num (table_to_list snd_t))))

let axis_labels x1 x2 scrx num =
  let rec range i j = if i >= j then [] else i::(range (i+1) j) in
  (int_of_float (interpolate 0. x1 x2 0. (float_of_int (scrx - 1))), "0.0")::
  (List.map (fun x ->
    (int_of_float (interpolate (float_of_int x) 0.0 (float_of_int(num-1))
		                 0.0 (float_of_int(scrx-1))),
     string_of_float (interpolate (float_of_int x) 0.0
			(float_of_int(num-1)) x1 x2)
    )) (range 0 num))
  
(* precondition: t has 2 columns, each of type int or float *)    
let vis_points2d t connect =
  let (xcol, ycol) = get_cols t in
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
  let last = ref (0, 0) in
  let first_line = ref true in
  List.iter2 (fun x y ->
    let scrx = int_of_float (interpolate x x1 x2 0. w) in
    let scry = int_of_float (interpolate y y1 y2 0. h) in
    if connect then (
      if not !first_line then (
        draw_line canvas (fst !last) (snd !last) scrx scry;
      );
      first_line := false;
      last := (scrx, scry)
    )) xcol ycol;
  List.iter2 (fun x y ->
    let scrx = int_of_float (interpolate x x1 x2 0. w) in
    let scry = int_of_float (interpolate y y1 y2 0. h) in
    draw canvas scrx scry '*')
    xcol ycol;
  print_endline (fst (List.nth (get_colnames t) 1));  (* print y-axis name *)
  print_canvas canvas (axis_labels x1 x2 canvas_width 4)
    (axis_labels y1 y2 canvas_height 4);
  print_endline (((String.make (10 + canvas_width / 2) ' ')) ^
    (fst (List.hd (get_colnames t))))
  
(* visualize [t] [v]: visualize the table [t] graphically using *)
(* the specified method [v]. *)
(* Throws an exception if the method is not compatible with *)
(* the table contents. *)
let visualize t = function
  | Scatter2d -> vis_points2d t false
  | LineGraph -> vis_points2d t true
  | VisNone -> print_tabular t
  | _ -> failwith "TODO"

(* legal_vis_methods [t] returns the legal visualization methods for *)
(* the table [t] *)
let legal_vis_methods t =
  match get_colnames t with
  | [(_, Int _)] | [(_, Float _)] ->
     [VisNone; Hist2d]
  | [(_, Int _); (_, Int _)] | [(_, Int _); (_, Float _)]
  | [(_, Float _); (_, Int _)] | [(_, Float _); (_, Float _)] ->
     [VisNone; Scatter2d; BarGraph; LineGraph]
  | [(_, String _); (_, Int _)] | [(_, String _); (_, Float _)] ->
     [VisNone; BarGraph]
  | _ -> [VisNone]

