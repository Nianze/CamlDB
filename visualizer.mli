(* represents supported visualization methods *)
type vis_method = Scatter2d | Hist2d | BarGraph

(* print_tabular [t] prints the table [t] to the console in tabular form *)
val print_tabular: table -> unit

(* visualize [t] [v]: visualize the table [t] graphically using *)
(* the specified method [v]. *)
(* Throws an exception if the method is not compatible with *)
(* the table contents. *)
val visualize: table -> vis_method -> unit

(* legal_vis_methods [t] returns the legal visualization methods for *)
(* the table [t] *)
val legal_vis_methods: table -> vis_method list
