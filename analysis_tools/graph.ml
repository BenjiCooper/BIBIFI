(* A directed graph implementation *)
(* Cyclomatic complexity (cc) for a graph = E - N + 2*P *)
(* Note: cc for a single method/function/procedure *)
(* is always E - N + 2. Within a single class, P is the  *)

exception NodeError of string

(* map and fold left *)
let map f l = (List.map f l);;
let foldl f a l = (List.fold_left f a l);;

(* size of a list *)
let list_size lst = foldl (fun a b -> a + 1) 0 lst;;

(* list contains method *)
let contains lst n = foldl (fun a b -> if b = n then a || true else a || false) false lst;;

(* Graph type definition *)
type node = int
type edge = { src : node; dst : node; }
type graph = { nodes : node list; edges : edge list; }

(* Return the result of inserting a new node into the graph *)
let insert_node gr n = 
	let nodes' = n::(gr.nodes) in
	{ nodes = nodes' ; edges = gr.edges }
;;

(* Return the result of adding an edge to the graph between two specified nodes *)
let insert_edge gr a b = 
	let a' = contains gr.nodes a in
	let b' = contains gr.nodes b in
	if a' && b' then
		let e = { src = a; dst = b; } in
		let edges' = e::gr.edges in
		{ nodes = gr.nodes; edges = edges' }
	else raise (NodeError "invalid node(s)")
	
(* number of nodes in a graph *)
let num_nodes gr = list_size (gr.nodes);;
(* number of edges in a graph *)
let num_edges gr = list_size (gr.edges);;

(* compute cyclomatic complexity for a graph *)
(* Note: P = number of methods in a class/file *)
let cyclomatic gr p = 
	let e = num_edges gr in
	let n = num_nodes gr in
	e - n + 2*p
;;