%parameter<Context : sig val ctx : ArkAst.Ctx.t Syntax.context end>
%start sygus_output
%type <[`Sat | `Unsat | `Unknown] * (string * 'a Syntax.formula) list * Syntax.symbol list> sygus_output

%{
open Syntax
open Context
module DynArray = BatDynArray
let symMap = DynArray.make 512 
let pp_pos formatter pos =
  let open Lexing in
  Format.fprintf formatter "File \"%s\", line %d, position %d"
		 pos.pos_fname
		 pos.pos_lnum
		 (pos.pos_cnum - pos.pos_bol + 1)
			   
let symbol_of_string =
  Memo.memo (fun name -> try  (snd (DynArray.get symMap (DynArray.index_of (fun ns -> (fst ns) = name) symMap)))
			 with Not_found ->
			   mk_symbol ctx ~name `TyReal)

%}

%token EOF
%token <string> ID
%token <QQ.t> REAL
%token ADD MINUS MUL
%token AND OR NOT
%token EQ LEQ GEQ LT GT
%token DISTINCT
%token LPAREN RPAREN
%token SAT UNSAT UNKNOWN
%token TINT TBOOL TREAL
%token DEFINE_FUN
%token TRUE FALSE
%%

/* %left ADD MUL */
/* %left OR */
/* %left AND */
/* %nonassoc NOT */
/* %nonassoc LPAREN */

sygus_formula:
  | LPAREN; phi = up_sygus_formula; RPAREN { phi }
  | TRUE {mk_true ctx}
  | FALSE {mk_false ctx}
;

up_sygus_formula:
  | NOT; phi = sygus_formula { mk_not ctx phi }
  | AND; conjuncts = list(sygus_formula) { mk_and ctx conjuncts }
  | OR; disjuncts = list(sygus_formula) { mk_or ctx disjuncts }
  | GEQ; s = sygus_term; t = sygus_term { mk_leq ctx t s }
  | GT; s = sygus_term; t = sygus_term { mk_lt ctx t s }
  | LEQ; s = sygus_term; t = sygus_term { mk_leq ctx s t }
  | LT; s = sygus_term; t = sygus_term { mk_lt ctx s t }
  | EQ; s = sygus_term; t = sygus_term { mk_eq ctx t s }
  | DISTINCT; s = sygus_term; t = sygus_term { mk_not ctx (mk_eq ctx s t)}

;

sygus_term:
  | LPAREN; t = up_sygus_term; RPAREN { t }
  |  v = ID { mk_const ctx (symbol_of_string v) }
  | k = REAL { mk_real ctx k }
;

up_sygus_term:
  | ADD; ts = list(sygus_term) { mk_add ctx ts }
  | MUL; ts = list(sygus_term) { mk_mul ctx ts }
  | MINUS; t = sygus_term { mk_neg ctx t }
  | k = REAL { mk_real ctx k }
;

sygus_sort:
  | TREAL {`TyReal}
  | TINT {`TyReal} (*game has only real variables so add it as real *)
  | TBOOL {`TyBool}
;

sygus_sorted_var:
  | LPAREN; v = ID; argtype = sygus_sort; RPAREN; {(v, argtype)} 
;

/* try ignore (DynArray.index_of (fun ns -> (fst ns) = name) symMap) */
/* 								with Not_found -> DynArray.add symMap (name, (mk_symbol ctx ~name t))) args} */
parse_args:
  | args = list(sygus_sorted_var); {List.iter (fun (name, t) -> DynArray.add symMap (name, (mk_symbol ctx ~name t))) args}
;


sygus_fun_spec:
/* | LPAREN; DEFINE_FUN; fun_name = ID; LPAREN; args = list(sygus_sorted_var); RPAREN; ret = sygus_sort; spec = sygus_formula; RPAREN; */
  | LPAREN; DEFINE_FUN; fun_name = ID; LPAREN; parse_args; RPAREN; ret = sygus_sort; spec = sygus_formula; RPAREN;
/* {let  funtyp = `TyFun (args, ret) in (Ctx.mk_symbol ~name:fun_name funtyp, spec)} */
    {(fun_name, spec)}
;

sygus_output:
  | SAT; EOF {(`Sat, [], [])}
  | UNKNOWN; EOF {(`Unknown, [], [])}
  | UNSAT; fspec = list(sygus_fun_spec); EOF {(`Unsat, fspec, (List.map (fun p -> (snd p)) (DynArray.to_list symMap)))}
;


