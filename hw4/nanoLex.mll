{
open Nano
open NanoParse
}

rule token = parse

 "let"{ LET } 
| "rec"{ REC }
| "="{ EQ }
| "in" { IN }
| "fun"{ FUN }
| "->"{ ARROW }
| "if"{ IF }
| "then"{ THEN }
| "else"{ ELSE }
| "+"   { PLUS }
| "-"   { MINUS }
| "*"{ MUL }
| "/" { DIV }
| "!="{ NE }
| "&&" { AND }
| "||" { OR }
| "false"{ FALSE }
| "true" {TRUE}
| "false" {FALSE}
| "(" {LPAREN}
| ")" {RPAREN}
| "<" {LT}
| "<=" {LE}
| "="{EQ}
| "::" {COLONCOLON}
| "[" {LBRAC}
| "]" {RBRAC}
|eof {EOF}

|['0'-'9']+ as x {Num(int_of_string x)}
|['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str{Id (str)}
|[' ' '\n' '\r' '\t'] {token lexbuf}

|_ as chr {failwith ("lex error:" ^(Char.escaped chr))}
