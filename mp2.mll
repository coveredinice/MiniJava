(* Header *)

{ open Mp3 }

 (* Definitions *)

(* Digits, letters*)
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let integers = ['0'-'9']+
(* White spaces and newlines *)
let spaces = ' ' | '\n' | '\t' | '\r' | "\r\n" 
(* Floats *)
let floats = digit+ '.' digit+ ('E' ('+' | '-')? digit+)?

rule main = parse 
 (* Spaces and new lines *)
 | spaces			{ main lexbuf }
 (* End of file *)
 | eof 				{ EOF }
 (* Keywords *)
 | "boolean"		{ BOOLEAN }
 | "class"			{ CLASS }
 | "else"			{ ELSE }
 | "extends"		{ EXTENDS }
 | "float"			{ FLOAT }
 | "if"				{ IF }
 | "int"			{ INT }
 | "new"			{ NEW }
 | "public"			{ PUBLIC }
 | "return"			{ RETURN }
 | "String"			{ STRING } 
 | "length"			{ LENGTH }
 | "this"			{ THIS }
 | "while"			{ WHILE }
 (* Parentheses and punctuation *)
 | '('				{ LPAREN }
 | ')'				{ RPAREN }
 | '{'				{ LBRACE }
 | '}'				{ RBRACE }
 | '['				{ LBRACK }
 | ']'				{ RBRACK }
 | ';'				{ SEMICOLON }
 | ','				{ COMMA }
 | '.'				{ DOT }
 | '[' [' ' '\t' '\n']* ']'	{ BRACKS }
 (* Operators *)
 | '='				{ EQ }
 | '<'				{ LT }
 | '!'				{ NOT }
 | "&&"				{ ANDAND }
 | "||"				{ OROR }
 | '+'				{ PLUS }
 | '-'				{ MINUS }
 | '*'			   	{ MULT }
 | '/'			   	{ DIV }
 (* Literals *)
 | "null"		   	{ NULL_LITERAL }
 | "true"		   	{ BOOLEAN_LITERAL true }
 | "false"        	{ BOOLEAN_LITERAL false }
 (* Integers and floats *)
 | integers as a	{ INTEGER_LITERAL (int_of_string a) }
 | floats as b 		{ FLOAT_LITERAL (float_of_string b)}
 (* Identifiers *)
 | letter(letter | digit)* as a 	{ IDENTIFIER a }
  (* String literals *)
 | '"'				{ lit_string lexbuf }
 (* Oneline comments *)
 | "//"  			{ oneline lexbuf }
 (* Multiline comments *)
 | "/*"             { multiline lexbuf }
 | "*/"             { failwith("The comment is not opened correctly") }
and lit_string = parse					  
 | ([^'\n''\\''"''\r']* as a ) '"'   { STRING_LITERAL a}
 and multiline = parse
 | "*/"              { main lexbuf}
 | _                 { multiline lexbuf}
 | eof               { failwith("The comment is not terminated correctly")}
and oneline = parse
 | '\n' | eof     { main lexbuf }
 | _                 { oneline lexbuf }
  

(* trailer *)
{

 let rec iter b = 
 	match main b with 
 		| EOF -> [] 
 		| t -> t :: (iter b)
;;

let fromstring s = iter (Lexing.from_string s)
let fromfile s = iter (Lexing.from_channel (open_in s))

}




