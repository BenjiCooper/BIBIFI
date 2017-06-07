(* Code to convert C code into a control flow graph. *)

#use "ast_c.ml"
#use "../graph.ml"

(* STATEMENT CONVERSION TO GRAPH *)

let rec stmt_to_graph st = match st with

      ExprSt(e) -> failwith "unimplemented"
    | Block(b) -> foldl (fun a b -> connect a (stmt_to_graph b)) empty_graph b

    (* THESE ARE THE IMPORTANT ONES *)
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
    | Switch(e,cl) -> failwith "unimplemented" (* All the pieces are there in my head, I just need to put them together. *)
    | While(e,s) -> let h = next () in
                    let g = stmt_to_graph s in
                    let t = next () in
                    let nodes = [h;t]@g.nodes in
                    let e1 = {src=h;dst=g.head;} in
                    let e2 = {src=g.tail;dst=h;} in
                    let e3 = {src=h;dst=t;} in
                    let edges = [e1;e2;e3]@g.edges in
                    { nodes = nodes; edges = edges; head = h; tail = t; }
    | DoWhile(s,e) -> let g = to_graph s in
                      let c = next () in
                      let t = next () in
                      let nodes = [c;t]@g.nodes in
                      let e1 = {src=g.tail;dst=c;} in
                      let e2 = {src=c;dst=g.head;} in
                      let e3 = {src=c;dst=t;} in
                      let edges = [e1;e2;e3]@g.edges in
                      { nodes = nodes; edges = edges; head = g.head; tail = t; }
    | For(e1,e2,e3,s) -> failwith "unimplemented"

    (* REALLY NO IDEA WHAT TO DO WITH THESE *)
    | Break -> failwith "unimplemented" 
    | Continue -> failwith "unimplemented"
    | Label(s,n) -> failwith "unimplemented"
    | Goto(n) -> failwith "unimplemented"

    (* Anything else just becomes a single node *)
    | _ -> let n = next () in
           { nodes = [n]; edges = []; head = n; tail = n; }
;;

(* TOP-LEVEL CONVERSION TO GRAPH *)

let func_to_graph { f_name = n; f_type = t; f_body = b; f_static = s; } = 
    foldl (fun a x -> connect a (stmt_to_graph x)) empty_graph b
;;

let rec top_level_to_graph tl = match tl with
      FuncDef(fd) -> func_to_graph fd
    | _ -> empty_graph

(* PROGRAM CONVERSION TO GRAPH *)

let rec program_to_graph pr = match pr with
      [] -> empty_graph
    | h::t -> let g1 = top_level_to_graph h in
              let g2 = program_to_graph t in
              connect g1 g2
;;

(* CODE CONVERSION TO GRAPH *)

let rec to_graph code = match code with
      Expr(ex) -> empty_graph
    | Stmt(st) -> stmt_to_graph st
    | Type(ty) -> empty_graph
    | Toplevel(tl) -> top_level_to_graph tl
    | Program(pr) -> program_to_graph pr
;;
