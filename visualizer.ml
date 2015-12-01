open Table
  
type vis_method = Scatter2d | Hist2d | BarGraph | LineGraph

let string_of_elt = function
  | Int x -> string_of_int x
  | String x -> x
  | Float x -> string_of_float x
  | Bool x -> if x then "True" else "False"
      
let calc_widths t =
  let title_widths = [List.map String.length titles] in
  let elt_widths = function
    | None -> []
    | Some n -> let row =
		  List.map (fun x -> String.length (string_of_elt !x)) n in
		row::(elt_widths n.next) in
  failwith "TODO"		    
  
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
    

(* visualize [t] [v]: visualize the table [t] graphically using *)
(* the specified method [v]. *)
(* Throws an exception if the method is not compatible with *)
(* the table contents. *)
let visualize t v =
  failwith "TODO"

(* legal_vis_methods [t] returns the legal visualization methods for *)
(* the table [t] *)
let legal_vis_methods t =
  failwith "TODO"    
