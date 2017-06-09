(* Code to convert Java code into a control-flow graph *)

(*#use "ast_java.ml"*)
#use "../graph.ml"

(* Statement to graph *)

let rec stmt_to_graph stmt = match stmt with
    (* Basic building blocks *)
      Empty -> empty_graph
    | Block(ss) -> foldr (fun a b -> connect a (stmt_to_graph b)) empty_graph ss
    | Expr(e) -> empty_graph (* Maybe put this with misc? *)

    (* Conditionals and loops *)
    | If(e,s1,s2) -> let h = next () in
                     let g1 = stmt_to_graph s1 in
                     let g2 = stmt_to_graph s2 in
                     let t = next () in
                     let nodes = [h;t]@g1.nodes@g2.nodes in
                     let e1 = {src=h;dst=g1.head;} in
                     let e2 = {src=h;dst=g2.head;} in
                     let e3 = {src=g1.tail;dst=t;} in
                     let e4 = {src=g2.tail;dst=t;} in
                     let edges = [e1;e2;e3;e4]@g1.edges@g2.edges in
                     { nodes = nodes; edges = edges; head = h; tail = t; }
    | Switch(e,cs) -> failwith "unimplemented" (* UGHHHHHH *)
    | While(e,s) -> let h = next () in
                    let g = stmt_to_graph s in
                    let t = next () in
                    let nodes = [h;t]@g.nodes in
                    let e1 = {src=h;dst=g.head;} in
                    let e2 = {src=g.tail;dst=h;} in
                    let e3 = {src=h;dst=t;} in
                    let edges = [e1;e2;e3]@g.edges in
                    { nodes = nodes; edges = edges; head = h; tail = t; }
    | Do(s,e) -> failwith "unimplemented" (* Is this just a do-while? *)
    | For(fc,s) -> let h = next () in
                   let g = stmt_to_graph s in
                   let t = next () in
                   let nodes = [h;t]@ g.nodes in
                   let e1 = {src=h;dst=g.head;} in
                   let e2 = {src=g.tail;dst=h;} in
                   let e3 = {src=h;dst=t;} in
                   let edges = [e1;e2;e3]@g.edges in
                   { nodes = nodes; edges = edges; head = h; tail = t; }

    (* ============================== *)
    (* Not sure what to do with these *)
    (* ============================== *)
    | Break(id) -> failwith "unimplemented"
    | Continue(id) -> failwith "unimplemented"
    | Return(ex) -> failwith "unimplemented"
    | Label(id,s) -> failwith "unimplemented"

    | Sync(ex,s) -> failwith "unimplemented"

    | Try(s1,cs,s2) -> failwith "unimplemented"
    | Throw(ex) -> failwith "unimplemented"

    | LocalVar(vwi) -> failwith "unimplemented"
    | Local_Class(cd) -> failwith "unimplemented"

    | Assert(e1,e2) -> failwith "unimplemented"

    (* Miscellanious cases *)
    | _ -> let n = next () in { nodes = [n]; edges = []; head = n; tail = n }

(* Method to graph *)

let rec method_to_graph meth = failwith "unimplemented"

(* Class to graph *)

let rec class_to_graph class = failwith "unimplemented"

(* Code to graph *)

let rec to_graph pr = match pr with
      AIdent(i) -> failwith "unimplemented"
    | AExpr(e) -> empty_graph
    | AStmt(s) -> stmt_to_graph s
    | AType(t) -> failwith "unimplemented"
    | AVar(v) -> failwith "unimplemented"
    | AInit(i) -> failwith "unimplemented"
    | AMethod(m) -> failwith "unimplemented"
    | AField(f) -> failwith "unimplemented"
    | AClass(c) -> failwith "unimplemented"
    | ADecl(d) -> failwith "unimplemented"
    | AProgram(p) -> failwith "unimplemented"


