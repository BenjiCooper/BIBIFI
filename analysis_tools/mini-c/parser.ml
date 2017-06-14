open Ast
open Utils

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let get_toks (a,b) =
    a

let get_expr (a,b) =
    b

let get_ID x =
    match x with
      Tok_ID s -> s
    | _ -> raise (InvalidInputException("Expected type ID."))

let lookahead toks =
    match toks with
      [] -> raise (InvalidInputException ("No tokens."))
    | h::t -> h




let rec parse_UnExp toks =
    let l = lookahead toks in
    match l with
      Tok_Not ->
        let t = match_token toks Tok_Not in
        let eval = parse_UnExp t in
        let t' = get_toks eval in
        let e = get_expr eval in
        (t', Not(e))
      | _ -> parse_PrimExp toks

and parse_PowExp toks =
    let eval1 = parse_UnExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Pow ->
        let t = match_token t1 Tok_Pow in
        let eval2 = parse_PowExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Pow(e1,e2))
    | _ -> (t1, e1)

and parse_MultExp toks =
    let eval1 = parse_PowExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Mult ->
        let t = match_token t1 Tok_Mult in
        let eval2 = parse_MultExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Mult(e1,e2))
    | Tok_Div ->
        let t = match_token t1 Tok_Div in
        let eval2 = parse_MultExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Div(e1,e2))
    | _ -> (t1, e1)

and parse_AddExp toks =
    let eval1 = parse_MultExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Plus ->
        let t = match_token t1 Tok_Plus in
        let eval2 = parse_AddExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Plus(e1,e2))
    | Tok_Sub ->
        let t = match_token t1 Tok_Sub in
        let eval2 = parse_AddExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Sub(e1,e2))
    | _ -> (t1, e1)

and parse_RelExp toks =
    let eval1 = parse_AddExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Greater ->
        let t = match_token t1 Tok_Greater in
        let eval2 = parse_RelExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Greater(e1,e2))
    | Tok_Less ->
        let t = match_token t1 Tok_Less in
        let eval2 = parse_RelExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Less(e1,e2))
    | Tok_GreaterEqual ->
        let t = match_token t1 Tok_GreaterEqual in
        let eval2 = parse_RelExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, GreaterEqual(e1,e2))
    | Tok_LessEqual ->
        let t = match_token t1 Tok_LessEqual in
        let eval2 = parse_RelExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, LessEqual(e1,e2))
    | _ -> (t1, e1)

and parse_EqExp toks =
    let eval1 = parse_RelExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Equal ->
        let t = match_token t1 Tok_Equal in
        let eval2 = parse_EqExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Equal(e1,e2))
    | Tok_NotEqual ->
        let t = match_token t1 Tok_NotEqual in
        let eval2 = parse_EqExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, NotEqual(e1,e2))
    | _ -> (t1, e1)


and parse_AndExp toks =
    let eval1 = parse_EqExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_And ->
        let t = match_token t1 Tok_And in
        let eval2 = parse_AndExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,And(e1,e2))
    | _ -> (t1, e1)


and parse_OrExp toks =
    let eval1 = parse_AndExp toks in
    let t1 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let l = lookahead t1 in
    match l with
      Tok_Or ->
        let t = match_token t1 Tok_Or in
        let eval2 = parse_OrExp t in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2,Or(e1,e2))
    | _ -> (t1, e1)

and parse_PrimExp toks =
    let l = lookahead toks in
    match l with
      Tok_Int x ->
        let t = match_token toks (Tok_Int x) in
        (t, Int x)
    | Tok_Bool x ->
        let t = match_token toks (Tok_Bool x) in
        (t, Bool x)
    | Tok_ID x ->
        let t = match_token toks (Tok_ID x) in
        (t, Id x)
    | Tok_LParen ->
        let t = match_token toks Tok_LParen in
        let eval = parse_OrExp t in
        let t1 = get_toks eval in
        let e = get_expr eval in
        let t2 = match_token t1 Tok_RParen in
        (t2, e)
    | _ -> raise (InvalidInputException(string_of_token l))

let rec parse_expr toks =
    parse_OrExp toks

let rec parse_DecStmt toks =
    let l = lookahead toks in
    match l with
      Tok_Type_Int ->
        let t1 = match_token toks Tok_Type_Int in
        let l' = lookahead t1 in
        let str = (get_ID l') in
        let t2 = match_token t1 (Tok_ID str) in
        let t3 = match_token t2 Tok_Semi in
        (t3, Declare(Type_Int, str))
    | Tok_Type_Bool ->
        let t1 = match_token toks Tok_Type_Bool in
        let l' = lookahead t1 in
        let str = (get_ID l') in
        let t2 = match_token t1 (Tok_ID str) in
        let t3 = match_token t2 Tok_Semi in
        (t3, Declare(Type_Bool, str))
    | _ -> raise (InvalidInputException("Expected type signature."))

and parse_AssignStmt toks =
    let l = lookahead toks in
    match l with
      Tok_ID s ->
        let t1 = match_token toks (Tok_ID s) in
        let t2 = match_token t1 Tok_Assign in
        let eval = parse_expr t2 in
        let t3 = get_toks eval in
        let t4 = match_token t3 Tok_Semi in
        let e = get_expr eval in
        (t4,Assign(s,e))
    | _ -> raise (InvalidInputException("Expected ID."))

and parse_PrintStmt toks =
    let t1 = match_token toks Tok_Print in
    let t2 = match_token t1 Tok_LParen in
    let eval = parse_expr t2 in
    let t3 = get_toks eval in
    let e = get_expr eval in
    let t4 = match_token t3 Tok_RParen in
    let t5 = match_token t4 Tok_Semi in
    (t5, Print(e))

and parse_IfStmt toks =
    let t1 = match_token toks Tok_If in
    let t2 = match_token t1 Tok_LParen in
    let eval1 = parse_expr t2 in
    let t3 = get_toks eval1 in
    let e1 = get_expr eval1 in
    let t5 = match_token t3 Tok_RParen in
    let t6 = match_token t5 Tok_LBrace in
    let eval2 = parse_stmt t6 in
    let t7 = get_toks eval2 in
    let stmt1 = get_expr eval2 in
    let t8 = match_token t7 Tok_RBrace in
    let l = lookahead t8 in
    match l with
      Tok_Else ->
        let t9 = match_token t8 Tok_Else in
        let t10 = match_token t9 Tok_LBrace in
        let eval3 = parse_stmt t10 in
        let t11 = get_toks eval3 in
        let stmt2 = get_expr eval3 in
        let t12 = match_token t11 Tok_RBrace in
        (t12, If(e1, stmt1, stmt2))
    | _ -> (t8, If(e1, stmt1, NoOp))

and parse_WhileStmt toks =
    let t1 = match_token toks Tok_While in
    let t2 = match_token t1 Tok_LParen in
    let eval1 = parse_expr t2 in
    let t3 = get_toks eval1 in
    let e = get_expr eval1 in
    let t4 = match_token t3 Tok_RParen in
    let t5 = match_token t4 Tok_LBrace in
    let eval2 = parse_stmt t5 in
    let t6 = get_toks eval2 in
    let stmt = get_expr eval2 in
    let t7 = match_token t6 Tok_RBrace in
    (t7, While(e,stmt))

and parse_stmt toks =
    let l = lookahead toks in
    match l with
      Tok_Type_Int ->
        let eval1 = parse_DecStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | Tok_Type_Bool ->
        let eval1 = parse_DecStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | Tok_ID s ->
        let eval1 = parse_AssignStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | Tok_Print ->
        let eval1 = parse_PrintStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | Tok_If ->
        let eval1 = parse_IfStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | Tok_While ->
        let eval1 = parse_WhileStmt toks in
        let t1 = get_toks eval1 in
        let e1 = get_expr eval1 in
        let eval2 = parse_stmt t1 in
        let t2 = get_toks eval2 in
        let e2 = get_expr eval2 in
        (t2, Seq(e1,e2))
    | _ -> (toks, NoOp)

let parse_main toks =
    let t1 = match_token toks Tok_Type_Int in
    let t2 = match_token t1 Tok_Main in
    let t3 = match_token t2 Tok_LParen in
    let t4 = match_token t3 Tok_RParen in
    let t5 = match_token t4 Tok_LBrace in
    let eval = parse_stmt t5 in
    let t6 = get_toks eval in
    let e = get_expr eval in
    let t7 = match_token t6 Tok_RBrace in
    let _ = match_token t7 EOF in
    e

