open Ast

let re_while = Str.regexp "while"
let re_type_int = Str.regexp "int"
let re_type_bool = Str.regexp "bool"
let re_sub = Str.regexp "-"
let re_semi = Str.regexp ";"
let re_RParen = Str.regexp ")"
let re_RBrace = Str.regexp "}"
let re_print = Str.regexp "printf"
let re_pow = Str.regexp "\\^"
let re_plus = Str.regexp "+"
let re_or = Str.regexp "||"
let re_NotEqual = Str.regexp "!="
let re_not = Str.regexp "!"
let re_mult = Str.regexp "*"
let re_main = Str.regexp "main"
let re_LessEqual = Str.regexp "<="
let re_less = Str.regexp "<"
let re_LParen = Str.regexp "("
let re_LBrace = Str.regexp "{"
let re_int = Str.regexp "-?[0-9]+"
let re_if = Str.regexp "if"
let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_GreaterEqual = Str.regexp ">="
let re_greater = Str.regexp ">"
let re_equal = Str.regexp "=="
let re_else = Str.regexp "else"
let re_div = Str.regexp "/"
let re_bool = Str.regexp "\\(true\\)\\|\\(false\\)"
let re_assign = Str.regexp "="
let re_and = Str.regexp "&&"
let re_whitespace = Str.regexp "[ \t\n]"

let tokenize input = 

	let str_to_bool s = (
		match s with
			| "true" -> true
			| "false" -> false
			| _ -> true
	) in

	let rec tok pos s = 
		if pos >= String.length s then
			[EOF]
		else begin
			if (Str.string_match re_while s pos) then
				Tok_While::(tok (pos+5) s)
			else if (Str.string_match re_type_int s pos) then
				Tok_Type_Int::(tok (pos+3) s)
			else if (Str.string_match re_type_bool s pos) then
				Tok_Type_Bool::(tok (pos+4) s)
			else if (Str.string_match re_sub s pos) then
				Tok_Sub::(tok (pos+1) s)
			else if (Str.string_match re_semi s pos) then
				Tok_Semi::(tok (pos+1) s)
			else if (Str.string_match re_RParen s pos) then
				Tok_RParen::(tok (pos+1) s)
			else if (Str.string_match re_RBrace s pos) then
				Tok_RBrace::(tok (pos+1) s)
			else if (Str.string_match re_print s pos) then
				Tok_Print::(tok (pos+6) s)
			else if (Str.string_match re_plus s pos) then
				Tok_Plus::(tok (pos+1) s)
			else if (Str.string_match re_or s pos) then
				Tok_Or::(tok (pos+2) s)
			else if (Str.string_match re_NotEqual s pos) then
				Tok_NotEqual::(tok (pos+2) s)
			else if (Str.string_match re_not s pos) then
				Tok_Not::(tok (pos+1) s)
			else if (Str.string_match re_mult s pos) then
				Tok_Mult::(tok (pos+1) s)
			else if (Str.string_match re_main s pos) then
				Tok_Main::(tok (pos+4) s)
			else if (Str.string_match re_LessEqual s pos) then
				Tok_LessEqual::(tok (pos+2) s)
			else if (Str.string_match re_less s pos) then
				Tok_Less::(tok (pos+1) s)
			else if (Str.string_match re_LParen s pos) then
				Tok_LParen::(tok (pos+1) s)
			else if (Str.string_match re_LBrace s pos) then
				Tok_LBrace::(tok (pos+1) s)
			else if (Str.string_match re_int s pos) then
				let token = Str.matched_string s in
				let new_pos = Str.match_end () in
				(Tok_Int (int_of_string token))::(tok new_pos s)
			else if (Str.string_match re_if s pos) then
				Tok_If::(tok (pos+2) s)
			else if (Str.string_match re_bool s pos) then
				let token = Str.matched_string s in
				let new_pos = Str.match_end () in
				(Tok_Bool (str_to_bool token))::(tok new_pos s)
			else if (Str.string_match re_GreaterEqual s pos) then
				Tok_GreaterEqual::(tok (pos+2) s)
			else if (Str.string_match re_greater s pos) then
				Tok_Greater::(tok (pos+1) s)
			else if (Str.string_match re_equal s pos) then
				Tok_Equal::(tok (pos+2) s)
			else if (Str.string_match re_div s pos) then
				Tok_Div::(tok (pos+1) s)
			else if (Str.string_match re_else s pos) then
				Tok_Else::(tok (pos+4) s)
			else if (Str.string_match re_assign s pos) then
				Tok_Assign::(tok (pos+1) s)
			else if (Str.string_match re_and s pos) then
				Tok_And::(tok (pos+2) s)
			else if (Str.string_match re_whitespace s pos) then
				tok (pos+1) s
			else if (Str.string_match re_pow s pos) then
				Tok_Pow::(tok (pos+1) s)
			else if (Str.string_match re_ID s pos) then
				let token = Str.matched_string s in
				let new_pos = Str.match_end () in
				(Tok_ID (token))::(tok new_pos s)
			else raise (InvalidInputException "invalid input")
		end
	in
	tok 0 input
;;

