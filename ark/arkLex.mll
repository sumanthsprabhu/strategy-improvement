{
open ArkParse
open Lexing
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

rule math_token = parse
| whitespace { math_token lexbuf }
| newline  { next_line lexbuf; math_token lexbuf }
| "Objective:" { OBJECTIVE }
| "And" { AND }
| "Or" { OR }
| "ForAll" { FORALL }
| "Exists" { EXISTS }
| "Not" { NOT }
| "<=" { LEQ }
| "=" { EQ }
| "<" { LT }
| "*" { MUL }
| "+" { ADD }
| "-" { MINUS }
| "," { COMMA }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| ['_' 'a'-'z' 'A'-'Z' '$' '?']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as lxm { ID(lxm) }
| ['-']?['0'-'9']+['/']?['0'-'9']* as lxm { REAL(QQ.of_string lxm) }
| eof { EOF }

and smt2token = parse
| whitespace { smt2token lexbuf }
| newline  { next_line lexbuf; smt2token lexbuf }
| "and" { AND }
| "or" { OR }
| "not" { SMT2NOT }
| "forall" { FORALL }
| "exists" { EXISTS }
| "<=" { LEQ }
| ">=" { GEQ }
| "=" { EQ }
| "<" { LT }
| ">" { GT }
| "*" { MUL }
| "+" { ADD }
| "-" { MINUS }
| "(" { LPAREN }
| ")" { RPAREN }
| ['_' 'a'-'z' 'A'-'Z' '$' '?']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as lxm { ID(lxm) }
| ['-']?['0'-'9']+['/']?['0'-'9']* as lxm { REAL(QQ.of_string lxm) }
| eof { EOF }

and game_token = parse
| whitespace { game_token lexbuf }
| newline  { next_line lexbuf; game_token lexbuf }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
| "<=" { LEQ }
| ">=" { GEQ }
| "=" { EQ }
| "<" { LT }
| ">" { GT }
| "*" { MUL }
| "+" { ADD }
| "-" { MINUS }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| "init:" { INIT }
| "safe:" { SAFE }
| "reach:" { REACH }
| "vars:" { VARS }
| ['_' 'a'-'z' 'A'-'Z' '$' '?']['_' 'a'-'z' 'A'-'Z' '0'-'9''\'']* as lxm { ID(lxm) }
| ['-']?['0'-'9']+['/']?['0'-'9']* as lxm { REAL(QQ.of_string lxm) }
| eof { EOF }

and sygus_output_token = parse
| whitespace { sygus_output_token lexbuf }
| newline  { next_line lexbuf; sygus_output_token lexbuf }
| "define-fun" { DEFINE_FUN }
| "sat" { SAT }
| "unsat" { UNSAT }
| "unknown" { UNKNOWN }
| "Int" { INT }
| "Bool" { BOOL }
| "and" { AND }
| "or" { OR }
| "not" { SMT2NOT }
| "<=" { LEQ }
| ">=" { LEQ }
| "=" { EQ }
| "<" { LT }
| ">" { GT }
| "*" { MUL }
| "+" { ADD }
| "-" { MINUS }
| "(" { LPAREN }
| ")" { RPAREN }
| ['_' 'a'-'z' 'A'-'Z' '$' '?']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as lxm { ID(lxm) }
| ['-']?['0'-'9']+['/']?['0'-'9']* as lxm { REAL(QQ.of_string lxm) }
| eof { EOF }
