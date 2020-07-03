type token = 
  | UNSAT
  | UNKNOWN
  | SAT
  | RPAREN
  | REAL of (QQ.t)
  | OR
  | NOT
  | MUL
  | MINUS
  | LT
  | LPAREN
  | LEQ
  | TINT
  | ID of (string)
  | GT
  | GEQ
  | EQ
  | EOF
  | DEFINE_FUN
  | TBOOL
  | TREAL
  | AND
  | ADD
  | DISTINCT
  | TRUE
  | FALSE

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}
