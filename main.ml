#load "mp3.cmo";;
#load "mp2.cmo";;
open Ast;;

	let parse_file f = Mp3.program Mp2.main (Lexing.from_channel (open_in f));;
	let parse_string s = Mp3.program Mp2.main (Lexing.from_string s);;