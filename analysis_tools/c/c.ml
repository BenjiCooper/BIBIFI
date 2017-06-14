(* Author: Benjamin Cooper, benji97@gmail.com
 * This code was written to perform analysis for code entries to the
 * Build it, Break it, Fix it (BIBIFI) competition.
 * This specific file converts C code into a control flow graph
 * in order to perform various analyses.
 *)

(* This should be done with modules, TODO: talk to Dr. Hicks and ask him to teach you modules please *)
#use "AST/ast_c.ml"
#use "../graph.ml"

(* STATEMENT CONVERSION TO GRAPH *)

let bhash1 = Hashtbl.create 20;; (* Used for contains_break *)
let bhash2 = Hashtbl.create 20;; (* Used for binding break statements *)
let ghash = Hashtbl.create 20;; (* Assume there's -hopefully- fewer than 20 GOTO statements *)

(* Does the given statement contain a break? *)
let rec contains_break stmt = match stmt with

      Block(b) -> foldl (fun a x -> a || (contains_break x)) false b

    | If(e,s1,s2) -> (contains_break s1) || (contains_break s2)
    (* NOTE: maybe switch should just return true? *)
    | Switch(e,cl) -> let case_to_break c = (match c with
                            Case(e,sl) -> foldl (fun a b -> a || (contains_break b)) false sl
                          | Default(sl) -> foldl (fun a b -> a || (contains_break b)) false sl) in 
        foldl (fun a b -> a || (case_to_break b)) false cl*)
    | While(e,s) -> contains_break s
    | DoWhile(s,e) -> contains_break s
    | For(e1,e2,e3,s) -> contains_break s

    | Continue -> true
    | Break -> true

    | Label(n,s) -> let u = Hashtbl.add bhash1 n s in contains_break s
    | Goto(n) -> let s = Hashtbl.find bhash1 n in contains_break s

    | _ -> false

let rec stmt_to_graph st = 

    let break = ref -1 in

    match st with

      Block(b) -> foldr (fun a x -> connect a (stmt_to_graph x)) empty_graph b

    (* Branch statements *)
    | If(e,s1,s2) -> let h = next () in
                     let g1 = stmt_to_graph s1 in
                     let g2 = stmt_to_graph s2 in
                     let t = next () in
                     let nodes = [h;t]@(g1.nodes)@(g2.nodes) in
                     let e1 = {src=h;dst=g1.head;} in
                     let e2 = {src=h;dst=g2.head;} in
                     let e3 = {src=g1.tail;dst=t;} in
                     let e4 = {src=g1.tail;dst=t;} in
                     let edges = [e1;e2;e3;e4]@(g1.edges)@(g2.edges) in
                     { nodes = nodes; edges = edges; head = h; tail = t; }
    | Switch(e,cl) -> let case_to_g c = (match c with
                             Default(sl) -> foldr (fun a b -> connect a (stmt_to_graph b)) empty_graph sl
                           | Case(e,sl) -> foldr (fun a b -> connect a (stmt_to_graph b)) empty_graph sl) in
                      let l = map (case_to_g) cl in
                      let h = next () in
                      let t = next () in
                      let g = {nodes=[h;t];edges=[];head=h;tail=t;} in
                      let btwn gr1 gr2 =
                          let nodes = gr1.nodes@gr2.nodes in
                          let e1 = {src=gr1.head;dst=gr2.head;} in
                          let e2 = {src=gr2.tail;dst=gr1.tail;} in
                          let edges = [e1;e2]@gr1.edges@gr2.edges in
                          { nodes = nodes; edges = edges; head = h; tail = t; }
                      in foldl (btwn) g l
    | While(e,s) -> (* Set up the graph to be returned *)
                    let h = next () in
                    let g = stmt_to_graph s in
                    let t = next () in
                    let nodes = [h;t]@g.nodes in
                    let e1 = {src=h;dst=g.head;} in
                    let e2 = {src=g.tail;dst=h;} in
                    let e3 = {src=h;dst=t;} in
                    let edges = [e1;e2;e3]@g.edges in
                    let graph = { nodes = nodes; edges = edges; head = h; tail = t; } in

                    (* Deal with any break statements *)
                    (* Currently does not support breaks in nested loops *)
                    let b = contains_break s in
                    let n = next () in
                    let u = if b then Hashtbl.add n t else () in
                    break := n; graph
    | DoWhile(s,e) -> let g = to_graph s in
                      let c = next () in
                      let t = next () in
                      let nodes = [c;t]@g.nodes in
                      let e1 = {src=g.tail;dst=c;} in
                      let e2 = {src=c;dst=g.head;} in
                      let e3 = {src=c;dst=t;} in
                      let edges = [e1;e2;e3]@g.edges in
                      { nodes = nodes; edges = edges; head = g.head; tail = t; }
    | For(ex1,ex2,ex3,s) -> let h = next () in
                            let g = stmt_to_graph s in
                            let t = next () in
                            let nodes = [h;t]@g.nodes in
                            let e1 = {src=h;dst=g.head;} in
                            let e2 = {src=g.tail;dst=h;} in
                            let e3 = {src=h;dst=t;} in
                            let edges = [e1;e2;e3]@g.edges in
                            {nodes = nodes; edges = edges; head = h; tail = t; }

    (* REALLY NO IDEA WHAT TO DO WITH THESE *)
    | Break -> let n = Hashtbl.find break in
               { nodes = [break]; edges = [{src=break;dst=n;}]; head = n; tail = n }
    | Continue -> failwith "unimplemented"

    (* Use a hash to create associations for Label and GOTO *)
    | Label(s,n) -> let c = next () in
                    let u = Hashtbl.add ghash n c in
                    { nodes = [n]; edges = []; head = n; tail = n; }
    | Goto(n) -> let n1 = Hashtbl.find ghash n in
                 let n2 = next () in
                 let e = {src=n2;dst=n1;} in
                 { nodes = [n]; edges = [e]; head = n; tail = n; }

    (* Anything else just becomes a single node *)
    (* NOTE: This probably won't work for ASM *)
    | _ -> let n = next () in
           { nodes = [n]; edges = []; head = n; tail = n; }
;;

(* TOP-LEVEL CONVERSION TO GRAPH *)

let func_to_graph { f_name = n; f_type = t; f_body = b; f_static = s; } = 
    foldr (fun a x -> connect a (stmt_to_graph x)) empty_graph b
;;

let rec top_level_to_graph tl = match tl with
      FuncDef(fd) -> func_to_graph fd
    | _ -> empty_graph
;;

(* PROGRAM CONVERSION TO GRAPH *)

(* This may be preferable. *)
let program_to_graph_list pr = map (fun a -> top_level_to_graph a) pr;;

(* This function is problematic, and shouldn't be used quite yet. *)
let rec program_to_graph pr = match pr with
      [] -> empty_graph
    | h::t -> let g1 = top_level_to_graph h in
              let g2 = program_to_graph t in
              (* This line is problematic, because of head and tail. Maybe I should return a graph list? *)
              { nodes = g1.nodes@g2.nodes; edges = g1.edges@g2.edges; head = g1.head; tail = g2.tail }
;;

(* CODE CONVERSION TO GRAPH *)

let rec to_graph code = match code with
      Expr(ex) -> empty_graph
    | Stmt(st) -> stmt_to_graph st
    | Type(ty) -> empty_graph
    | Toplevel(tl) -> top_level_to_graph tl
    | Program(pr) -> program_to_graph pr
;;


(* ========================================================================================================== *)
(* TODO: add code to read in a file, find cyclomatic complexity (and any other heuristic), and write to a csv *)
(* ========================================================================================================== *)

let analyze file_name = failwith "unimplemented" (* This should probably be in a separate file. *)

