%{
	open Ast
%}

/* Declarations */

%token EOF
%token BOOLEAN
%token CLASS 
%token EXTENDS 
%token PUBLIC
%token THIS
%token NEW
%token RETURN
%token STRING
%token LENGTH
%token IF 
%token ELSE
%token WHILE
%token FLOAT
%token INT
%token SEMICOLON
%token COMMA
%token DOT
%token OROR ANDAND LT PLUS MINUS NOT MULT DIV
%token EQ
%token LPAREN RPAREN 
%token LBRACE RBRACE
%token LBRACK RBRACK 
%token BRACKS
%token NULL_LITERAL
%token <bool> BOOLEAN_LITERAL
%token <int> INTEGER_LITERAL
%token <float> FLOAT_LITERAL
%token <string> IDENTIFIER
%token <string> STRING_LITERAL

%right ELSE
%left OROR
%left ANDAND
%left LT 
%left PLUS MINUS
%left MULT DIV
%right NOT
%left DOT LBRACK

%start program  
%type <Ast.program> program

%%

program:
	classdecls EOF			{ Program $1 }
;

classdecls:
	classdecl 				{ [$1] }
|	classdecls classdecl 	{ $1 @ [$2]	}
;

classdecl:
	CLASS IDENTIFIER EXTENDS IDENTIFIER LBRACE vardecls methdecls RBRACE	{ Class ($2, $4, $6, $7) }
|	CLASS IDENTIFIER LBRACE vardecls methdecls RBRACE						{ Class ($2, "Object", $4, $5) }
;

vardecls: 
| 	/* empty */ 					{ [] }
| 	vardecls vardecl  				{ $1 @ [$2] }
;

vardecl:
	type_p IDENTIFIER SEMICOLON 	{ Var ($1, $2) }
;

methdecls: 
| 	/* empty */ 					{ [] } 
| 	methdecls methdecl   			{ $1 @ [$2]	}
;

methdecl:
	PUBLIC type_p IDENTIFIER LPAREN type_p IDENTIFIER up RPAREN LBRACE vardecls stms RETURN expr SEMICOLON RBRACE		 { Method ($2, $3, [($5,$6)]@$7, $10, $11, $13)}
| 	PUBLIC type_p IDENTIFIER LPAREN RPAREN LBRACE vardecls stms RETURN expr SEMICOLON RBRACE		 { Method ($2, $3, [], $7, $8, $10)  }	
;

stms: 
|  	/* empty */			{ [] } 
| 	stm stms  			{ [$1] @ $2 }
;

up: 
|	/* empty */											{ [] }
|	COMMA type_p IDENTIFIER up							{ [($2, $3)] @ $4 }
;

type_p:
	type_p BRACKS										{ ArrayType $1 }
|	BOOLEAN 											{ BoolType }
| 	STRING 												{ StringType }
| 	FLOAT 												{ FloatType }
| 	INT 									 			{ IntType }
| 	IDENTIFIER											{ ObjectType $1	}
;

stm:
| 	LBRACE stms RBRACE									{ Block $2 }
| 	IF LPAREN expr RPAREN stm ELSE stm 					{ If ($3, $5, $7) }
| 	IF LPAREN expr RPAREN stm %prec ELSE				{ If ($3, $5, Block ([])) }
| 	WHILE LPAREN expr RPAREN stm 						{ While ($3,$5)	}
| 	IDENTIFIER EQ expr SEMICOLON 						{ Assignment ($1, $3) }
|	IDENTIFIER LBRACK expr RBRACK EQ expr SEMICOLON		{ ArrayAssignment ($1, $3, $6) }
;

expr:
	expr LT expr										{ Operation	($1, LessThan,$3) }
|	expr ANDAND expr									{ Operation ($1, And, $3) }
|	expr OROR expr										{ Operation ($1, Or, $3) }
|	expr PLUS expr										{ Operation ($1, Plus, $3) }
|	expr MINUS expr										{ Operation ($1, Minus, $3) }
|	expr MULT expr										{ Operation ($1, Mult, $3) }
|	expr DIV expr										{ Operation ($1, Div, $3) }
| 	expr LBRACK expr RBRACK								{ Array ($1, $3) }
|	expr DOT LENGTH 									{ Length $1	}
|	expr DOT IDENTIFIER LPAREN expr mixed RPAREN		{ MethCall ($1,$3,[$5]@$6) }
|	expr DOT IDENTIFIER LPAREN RPAREN					{ MethCall ($1,$3,[]) }
| 	NOT expr 											{ Not $2 }
| 	INTEGER_LITERAL										{ Integer $1 }
| 	FLOAT_LITERAL										{ Float $1 }
| 	STRING_LITERAL										{ String $1 }
| 	NULL_LITERAL										{ Null }
| 	BOOLEAN_LITERAL										{ if $1=true then True else False }
| 	IDENTIFIER											{ Id $1 }
| 	THIS 												{ This }
| 	NEW type_p LBRACK expr RBRACK 						{ NewArray ($2, $4) }
| 	NEW IDENTIFIER LPAREN RPAREN						{ NewId $2 }
| 	LPAREN expr RPAREN 									{ $2 }
;

mixed:  
|	/* empty */											{ [] } 
| 	COMMA expr mixed									{ [$2] @ $3 } 
;

%%