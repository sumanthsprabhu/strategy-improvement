open Syntax

let parseOutputFile : 'a context -> string  -> symbol list -> [`Sat | `Unsat | `Unknown] * 'a formula list =
  fun ark_ctx outputFileName xs ->
  let tmp =  "cat " ^ outputFileName in
  Unix.system tmp;
  let module SygusParser = SygusParse.Make(struct let ctx = ark_ctx end) in
  let open Lexing in
  let lexbuf = Lexing.from_channel (open_in outputFileName) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = outputFileName };
  let (result, rel2spec, args) = try SygusParser.sygus_output SygusLex.sygus_output_token lexbuf with
                             | _ ->
                                let pos = lexbuf.lex_curr_p in
                                failwith (Printf.sprintf "Result Parse error: %s:%d:%d"
                                            outputFileName
                                            pos.pos_lnum
                                            (pos.pos_cnum - pos.pos_bol + 1))
  in
  match result with
  | `Sat -> (`Sat, [])
  | `Unknown -> (`Unknown, [])
  | `Unsat -> (* Log.logf ~level:`always "Length of xs %d\n Length of args %d" (List.length xs) (List.length args);
               * List.iter (fun (r, s) -> Log.logf ~level:`always "relation %s, spec %s" r (Formula.show ark_ctx s)) rel2spec; *)
              let rec rsubst sym lp =
                match lp with
                | (largs,[]) -> Log.logf ~level:`always "resetting"; rsubst sym (largs, xs)
                | (a::lags, x::lxs) -> if a = sym then (mk_const ark_ctx x) else rsubst sym (lags, lxs)
                | ([], _) -> assert false in
              let subst sym = rsubst sym (args, xs) in
              (`Unsat, List.map (fun rs -> substitute_const ark_ctx subst (snd rs)) rel2spec)

let solve_cvc4 : 'a context -> symbol list ->  ('a, 'typ) expr list -> symbol list -> ('a, 'typ) expr list -> [`Sat | `Unsat | `Unknown] * 'a formula list =
  fun ark_ctx xs allVars relations constraints ->
  (* let solve ark_ctx xs allVars relations constraints = *)
  let fo_typ_symbol sym =
    match typ_symbol ark_ctx sym with
    | `TyInt -> `TyInt
    | `TyReal -> `TyReal
    | `TyBool -> `TyBool
    | _ -> assert false
  in
  let pp_farg formatter = function
    | (`TyReal, x) -> Format.fprintf formatter "(%s Int)" (symbol_name_unr ark_ctx x)
    | (`TyInt, x) -> Format.fprintf formatter "(%s Int)" (symbol_name_unr ark_ctx x)
    | (`TyBool, x) -> Format.fprintf formatter "(%s Bool)" (symbol_name_unr ark_ctx x)

  in
  let pp_space formatter () = Format.fprintf formatter " " in
  let pp_newline formatter () = Format.fprintf formatter "\n" in
  let pp_symbol_unr formatter x = Format.fprintf formatter "%s" (symbol_name_unr ark_ctx x) in
  let (inputFileName, inputFile) = Filename.open_temp_file "simsat_sygus_" ".sl" in
  let (outputFileName, outputFile) = Filename.open_temp_file "simsat_sygus_" ".output" in 
  let inputFileFormat = Format.formatter_of_out_channel inputFile in
  Format.fprintf inputFileFormat "(set-logic LIA)\n";
  List.iter (fun rel ->
      Format.fprintf inputFileFormat
        "(synth-fun %a (%a) Bool
         ((Start Bool ((and Start Start)
         (or  Start Start)
         (not Start)
         (<=  StartInt StartInt)
         (=   StartInt StartInt)
         (>=  StartInt StartInt)
         (> StartInt StartInt)))
         (StartInt Int (%a
         0
         1
         -1
         (+ StartInt StartInt)
         (- StartInt StartInt)
         (ite Start StartInt StartInt)))))"
        (pp_symbol_smtlib2 ark_ctx) rel
        (ArkUtil.pp_print_enum ~pp_sep:pp_space pp_farg) (BatList.enum (List.combine (List.map fo_typ_symbol xs) xs))
        (ArkUtil.pp_print_enum ~pp_sep:pp_newline pp_symbol_unr) (BatList.enum xs))
    relations;
  List.iter (fun var ->
      Format.fprintf inputFileFormat "(declare-var %a Int)\n"
        (pp_expr_smtlib2 ark_ctx) var)
    allVars;
  List.iter (fun c ->
      Format.fprintf inputFileFormat "(constraint %a)\n"
        (pp_expr_smtlib2 ark_ctx) c)
    constraints;
  Format.fprintf inputFileFormat "(check-synth)\n";
  close_out inputFile;
  let cvc4Cmd = " cat " ^ inputFileName ^ " && cvc4 " ^ inputFileName ^ " > " ^ outputFileName in
  (* List.iter (fun x -> match symbol_name ark_ctx x with
   *                     | Some name -> Log.logf ~level:`always "symbols in ark: %s\n" name
   *                     | None -> Log.logf ~level:`always "no symbol name") xs ; *)
  (* List.iter (fun x ->
   *     (\* let name = (symbol_name ark_ctx x) in *\)
   *     ignore (ArkAst.Ctx.mk_symbol ~name:"y_0" `TyReal)) xs; *)
  match Unix.system cvc4Cmd with
  | Unix.WEXITED 0 -> (parseOutputFile ark_ctx outputFileName xs)
     (* begin match  with
      * | (`Sat, _) -> (`Sat, [])
      * | (`Unknown, _) -> (`Unknown, [])
      * | (`Unsat, specs) -> (`Unsat, specs) *)
        (* Log.logf ~level:`always "SyGuS is UNSAT!";
         * let specs = 
         * List.iter (fun rs -> Log.logf ~level:`always "%a" (pp_symbol_smtlib2 ark_ctx) (get_named_symbol ark_ctx (fst rs)))  rel2spec *)
     (* List.iter (fun rs -> Log.logf ~level:`always "%s: %s" (fst rs) (Formula.show ark_ctx (snd rs))) rel2spec *)
     (* end *)
  | Unix.WEXITED retcode -> Log.logf ~level:`always "CVC4 timedout with retcode: %d\n" retcode; (`Unknown, [])
  | Unix.WSIGNALED signal -> Log.logf ~level:`always "CVC4 killed by a signal. Signal number: %d\n" signal; (`Unknown, [])
  | Unix.WSTOPPED signal -> Log.logf ~level:`always "CVC4 stopped by a signal. Signal number: %d\n" signal; (`Unknown, [])

(* let solve_freqn : 'a context -> symbol list ->  ('a, 'typ) expr list -> symbol list -> ('a, 'typ) expr list -> [`Sat | `Unsat | `Unknown] * 'a formula list =
 *   fun ark xs chcstr -> *)
let solve_freqn ark xs chcstr =
  let (inputFileName, inputFile) = Filename.open_temp_file "simsat_freqn_" ".smt2" in
    let (outputFileName, outputFile) = Filename.open_temp_file "simsat_freqn_" ".output" in 
    let inputFileFormat = Format.formatter_of_out_channel inputFile in
    Format.fprintf inputFileFormat "\n%s\n" chcstr;
    close_out inputFile;
    let freqnCmd = " cat " ^ inputFileName ^ "&& /home/sas20/aeval-modver/build/tools/nonlin/freqn " ^ inputFileName ^ " > " ^ outputFileName^ " && cat " ^ outputFileName
    in
    match Unix.system freqnCmd with
    | Unix.WEXITED 0 -> begin
        match parseOutputFile ark outputFileName xs with 
        | (`Sat, _) -> `Sat
        | (`Unknown, _) -> `Unknown
        | (`Unsat, specs) -> `Unsat specs
      end
    | Unix.WEXITED retcode -> Log.logf ~level:`always "CVC4 timedout with retcode: %d\n" retcode; `Unknown
    | Unix.WSIGNALED signal -> Log.logf ~level:`always "CVC4 killed by a signal. Signal number: %d\n" signal; `Unknown
    | Unix.WSTOPPED signal -> Log.logf ~level:`always "CVC4 stopped by a signal. Signal number: %d\n" signal; `Unknown

