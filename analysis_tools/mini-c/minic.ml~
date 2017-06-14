(* This version of mini-C is a coding language used in CMSC330 at the University of Maryland, College Park *)
(* used to teach students about operational semantics and code interpretation. *)
(* It is similar to C. *)
(* This code is a proof of concept, which converts Mini-C programs into control flow graphs (CFGs) *)
(* in order to run analyses (specifically, finding cyclomatic complexity) for those programs *)

open Graph
open Ast
open Parser
open Utils


(* Convert an evaluation statement into a control flow graph. *)
(* No Operation generates an empty graph *)
(* Sequencing statements should generate 2 graphs and connect them *)
(* If statements should create a branch in two directions *)
(* While loops should create a branch off the guard *)
(* Anything else should create a single node *)
let rec to_graph stmt = match stmt with
    | Seq(s1,s2) -> let g1 = to_graph s1 in
                    let g2 = to_graph s2 in
                    connect g1 g2
    | If(exp,s1,s2) -> let h = next () in
                       let l = to_graph s1 in
                       let r = to_graph s2 in
                       let t = next () in
                       let nodes = ([h;t]@(l.nodes)@(r.nodes)) in
                       let e1 = {src=h;dst=l.head;} in
                       let e2 = {src=h;dst=r.head} in
                       let e3 = {src=l.tail;dst=t} in
                       let e4 = {src=r.tail;dst=t} in
                       let edges = ([e1;e2;e3;e4]@(l.edges)@(r.edges)) in
                       { nodes = nodes; edges = edges; head = h; tail = t }
    | While(exp,s1) -> let g = next () in
                       let s = to_graph s1 in
                       let t = next () in
                       let nodes = ([g;t]@(s.nodes)) in
                       let e1 = {src=g;dst=s.head} in
                       let e2 = {src=s.tail;dst=g} in
                       let e3 = {src=g;dst=t;} in
                       let edges = ([e1;e2;e3]@(s.edges)) in
                       { nodes = nodes; edges = edges; head = g; tail = t; }
    | _ -> let n = next () in
           { nodes = [n]; edges = []; head = n; tail = n; }

let txt = ref ""
let read_file file =
    txt := "";
    let chan = open_in file in
    try 
        while true; do
            txt := !txt^(input_line chan)
        done;
    with End_of_file ->
        close_in chan;
        ()
;;

let parse_from_file file = 
    let code = read_file file; !txt in
    let toks = (Lexer.tokenize code) in
    parse_main toks

let build file = 
    let ast = parse_from_file file in
    let g = to_graph ast in
    g
;;

let () = 
    let fn = Array.get (Sys.argv) 0 in
    let g = build fn in
    print_graph g
;;
