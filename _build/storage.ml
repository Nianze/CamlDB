(* Json Format:
  {
    "name": (name of table)
    "rows": (number of rows)
    "cols": (number of columns)
    "header": [name1,type1,..,nameN,typeN]
    "elements": [a11,...,a1m, .., an1,...,anm]
  }

  "type" can be of "Int", "String", "Float" or "Bool"
*)

open Table

let save_table t filename =
  let header =
    let string_of_elt_typ = function
      | Int _ -> "Int"
      | String _ -> "String"
      | Float _ -> "Float"
      | Bool _ -> "Bool" in
    let make_nametype (n, t) =
      [`String n; `String (string_of_elt_typ t)] in
    [`As] @ (List.flatten (List.map make_nametype t.colnames)) @ [`Ae] in
  let elements =
    let lexeme_of_elt = function
      | Int n -> `Float (float_of_int n)
      | String s -> `String s
      | Float f -> `Float f
      | Bool b -> `Bool b in
    let rec make_rows = function
      | None -> []
      | Some n ->
	 (List.map (fun x -> lexeme_of_elt !x) n.value) @ make_rows n.next in
    make_rows t.first in    
  let lexemes = [`Os;
		`Name "name"; `String t.name;
		`Name "rows"; `Float (float_of_int t.numrow);
		`Name "cols"; `Float (float_of_int t.numcol);
		`Name "header"; `As] @ header @ [`Ae] @
		[`Name "elements"; `As] @ elements @ [`Ae] @
		[`Oe] in
  let ch = open_out filename in
  let enc = Jsonm.encoder ~minify:false (`Channel ch) in
  let _ = List.map (fun x -> Jsonm.encode enc (`Lexeme x)) lexemes in
  let _ = Jsonm.encode enc `End in
  close_out ch

let col_typ_of_string = function
  | "Int" -> Int 0
  | "String" -> String ""
  | "Float" -> Float 0.0
  | "Bool" -> Bool true
  | n -> failwith ("Invalid column type: " ^ n)

let elt_of_lexeme elt col_t =
  match (elt, col_t) with
  | (`Float f, Float _) -> Float f
  | (`Float n, Int _) -> Int (int_of_float n)
  | (`String s, _) -> String s
  | (`Bool b, _) -> Bool b
  | _ -> failwith "Invalid lexeme"

let build_table name rows cols header elts =
  let t = { name = name;
	    colnames = header;
	    numcol = cols;
	    numrow = 0;
	    first = None;
	    last = None } in
  let to_node row =
    let de_lexemized =
      List.map2 (fun elt (_, t) -> ref (elt_of_lexeme elt t)) row header in
    { prev = None; next = None; value = de_lexemized } in
  let _ = List.map (fun x -> insert (to_node x) t) elts in
  t

let load_table filename =
  let fin = open_in filename in
  let decoder = Jsonm.decoder (`Channel fin) in

  let next_lexeme () =
    match Jsonm.decode decoder with
    | `Lexeme l -> Some l
    | `End -> None
    | _ -> failwith ("Error loading table " ^ filename) in

  let force_next_lexeme () =
    match next_lexeme () with
    | Some x -> x
    | None -> failwith "No more lexemes" in

  let next_string () =
    match force_next_lexeme () with
    | `String s -> s
    | _ -> failwith "Not a string" in

  let tname = ref "" in
  let rows = ref 0 in
  let cols = ref 0 in
  let header = ref [] in
  let elts = ref [] in
  let cur_name = ref "" in

  let process l =
    match (l, !cur_name) with
    | (`Name "elements", _) ->
       let _ = force_next_lexeme () in   (* array start tag *)
       for i = 1 to !rows do
         let row = ref [] in
	 for j = 1 to !cols do
	   row := (force_next_lexeme ())::!row
	 done;
	 elts := (List.rev !row)::!elts
       done
    | (`Name n, _) -> cur_name := n
    | (`String s, "name") -> tname := s
    | (`Float f, "rows") -> rows := int_of_float f
    | (`Float f, "cols") -> cols := int_of_float f
    | (`String name, "header") ->
       let typ = next_string () in
       header := List.append !header [(name, col_typ_of_string typ)]
    | (`Os, _) | (`Oe, _) | (`As, _) | (`Ae, _) -> ()
    | _ -> failwith ("Invalid table file " ^ filename) in

  let running = ref true in
  while !running do
    match next_lexeme () with
    | Some l -> process l
    | None -> running := false
  done;

  build_table !tname !rows !cols !header !elts

