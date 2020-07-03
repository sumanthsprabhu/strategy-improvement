{
open SygusToken
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

rule sygus_output_token = parse
| whitespace { sygus_output_token lexbuf }
| newline  { next_line lexbuf; sygus_output_token lexbuf }
| "define-fun" { DEFINE_FUN }
| "sat" { SAT }
| "unsat" { UNSAT }
| "unknown" { UNKNOWN }
| "Real" { TREAL }
| "Int" { TINT }
| "Bool" { TBOOL }
| "and" { AND }
| "or" { OR }
| "not" { NOT }
| "true" { TRUE}
| "false" { FALSE }
| "<=" { LEQ }
| ">=" { LEQ }
| "=" { EQ }
| "distinct" { DISTINCT }
| "<" { LT }
| ">" { GT }
| "*" { MUL }
| "+" { ADD }
| "-" { MINUS }
| "(" { LPAREN }
| ")" { RPAREN }
| ['_' 'a'-'z' 'A'-'Z' '$' '?']['_' 'a'-'z' 'A'-'Z' '!' '0'-'9']* as lxm { ID(lxm) }
| ['-']?['0'-'9']+['/' '.']?['0'-'9']* as lxm { REAL(QQ.of_float (float_of_string lxm)) }
| eof { EOF }
