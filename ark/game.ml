open Syntax
open Smt
open BatPervasives

include Log.Make(struct let name = "ark.game" end)

module GameTree : sig
  type 'a t
  type 'a vertex
  val empty : 'a context ->
    (symbol list * symbol list) ->
    start:('a formula) -> 
    safe:('a formula) ->
    reach:('a formula) ->
    'a t
  val well_labeled : 'a t -> bool
  (* val expand_vertex : 'a t -> 'a vertex -> int -> bool *)
  val expand_vertex : ArkAst.Ctx.t t -> ArkAst.Ctx.t vertex -> int -> bool
  (* val expand_vertex : unit t -> unit vertex -> int -> bool                    *)

  val get_open : 'a t -> ('a vertex) option
  val root : 'a t -> 'a vertex
  val pp : Format.formatter -> 'a t -> unit
end = struct
  module A = BatDynArray

  type 'a vertex_state =
    | Covered of 'a vertex
    | Expanded of (('a vertex) list)
    | Open
  and 'a vertex =
    { id : int;
      mutable annotation : 'a formula;
      mutable state : 'a vertex_state;
      mutable covers : 'a vertex list;
      parent : ('a vertex * 'a formula * (('a, typ_fo) expr list)) option }

  type 'a t =
    { ark : 'a context;
      xs : symbol list;
      ys : symbol list;
      safe : 'a formula;
      reach : 'a formula;
      start : 'a formula;
      root : 'a vertex;
      var_to_level : (symbol * int, symbol) Hashtbl.t;
      level_to_var : (symbol, symbol) Hashtbl.t;
      mutable max_vertex : int }

  let root game_tree = game_tree.root

  let empty ark (xs,ys) ~start ~safe ~reach =
    assert (List.length xs == List.length ys);
    let root =
      { id = 0;
        annotation = mk_true ark;
        state = Open;
        covers = [];
        parent = None }
    in
    { ark = ark;
      xs = xs;
      ys = ys;
      safe = safe;
      reach = reach;
      start = start;
      root = root;
      max_vertex = 0;
      var_to_level = Hashtbl.create 991;
      level_to_var = Hashtbl.create 991 }

  let var_to_level game_tree level sym =
    let ark = game_tree.ark in
    try Hashtbl.find game_tree.var_to_level (sym, level)
    with Not_found ->
      begin
        let sym_level =
          mk_symbol
            ark
            ~name:((show_symbol ark sym) ^ "$" ^ (string_of_int level))
            (typ_symbol ark sym)
        in
        Hashtbl.add game_tree.var_to_level (sym, level) sym_level;
        Hashtbl.add game_tree.level_to_var sym_level sym;
        sym_level
      end
    
  let substitute_var_to_level game_tree level =
    let ark = game_tree.ark in
    substitute_const ark (mk_const ark % var_to_level game_tree level)

  let substitute_level_to_var game_tree =
    let ark = game_tree.ark in
    let f sym =
      try mk_const ark (Hashtbl.find game_tree.level_to_var sym)
      with Not_found -> assert false
    in
    substitute_const ark f

  let dimension game_tree = List.length game_tree.xs

  let nb_vertex game_tree = game_tree.max_vertex + 1

  let get_open game_tree =
    let rec find v =
      match v.state with
      | Covered _ -> None
      | Open -> Some v
      | Expanded children ->
        List.fold_left (fun open_vertex child ->
            match open_vertex with
            | Some v -> Some v
            | None -> find child)
          None
          children
    in
    find game_tree.root

  let children vertex =
    match vertex.state with
    | Expanded children -> children
    | _ -> []

  let rec depth vertex =
    match vertex.parent with
    | Some (p, _, _) -> 1 + depth p
    | None -> 0

  let rec height vertex =
    1 + (List.fold_left max 0 (List.map height (children vertex)))

  (* Verify well-labeledness conditions *)
  let well_labeled game_tree =
    let ark = game_tree.ark in
    let entails phi psi =
      Smt.entails game_tree.ark ~theory:"QF_LRA" phi psi = `Yes
    in
    let rec well_labeled_vertex v =
      let child_guards =
        children v |> List.map (fun c -> match c.parent with
            | None -> assert false
            | Some (_, guard, _) -> guard)
      in
      let consecution_and_availability =
        match v.parent with
        | None -> true
        | Some (parent, guard, move) ->
          (* Consecution: if u -[psi : m]-> v, then
               Phi(u)(x) /\ psi(x) /\ y = m(x) /\ reach(y,x') |= Phi(v)(x')
           *)
          let move_formula =
            List.map2
              (fun y m ->
                 match Expr.refine ark m with
                 | `Term t -> mk_eq ark (mk_const ark y) t
                 | `Formula phi -> mk_iff ark (mk_const ark y) phi)
              game_tree.ys
              move
          in
          let rename_xs =
            let f sym =
              (if List.mem sym game_tree.xs then
                 var_to_level game_tree 0 sym
               else
                 sym)
              |> mk_const ark
            in
            substitute_const ark f
          in
          let reach_move = rename_xs game_tree.reach in
          entails
            (mk_and ark ([parent.annotation; guard; reach_move]@move_formula))
            (rename_xs v.annotation)
          (* Availability: psi(x) /\ y = m(x) |= safe(x,y) *)
          && entails (mk_and ark (guard::move_formula)) game_tree.safe

      in
      consecution_and_availability
      && match v.state with
      | Expanded children ->
        entails v.annotation (mk_or ark child_guards) (* Adequacy *)
        && List.for_all well_labeled_vertex children
      | Covered u ->
        entails v.annotation u.annotation (* Covering *)
      | Open -> true
    in
    well_labeled_vertex game_tree.root

  let delete_children game_tree vertex =
    let rec delete vertex =
      begin match vertex.state with
        | Expanded children ->
          List.iter delete children
        | Covered covering ->
          covering.covers <-
            List.filter (fun v -> v.id != vertex.id) covering.covers
        | Open -> ()
      end;
      List.iter (fun v -> v.state <- Open) vertex.covers
    in
    match vertex.state with
    | Expanded children -> List.iter delete children
    | _ -> assert false

  let fo_typ_symbol ark sym =
    match typ_symbol ark sym with
    | `TyInt -> `TyInt
    | `TyReal -> `TyReal
    | `TyBool -> `TyBool
    | _ -> assert false
  let rec pp_prefix_matrix ark matrix = function          
    | (`Forall, x)::xs -> mk_forall ark ~name:(show_symbol_smtlib2 ark x) (fo_typ_symbol ark x) (pp_prefix_matrix ark matrix xs)
    | (`Exists, x)::xs -> mk_exists ark ~name:(show_symbol_smtlib2 ark x) (fo_typ_symbol ark x) (pp_prefix_matrix ark matrix xs)
    | [] -> matrix                  
  let pp_prefix ark formatter prefix =
    let pp_elt formatter = function
      | (`Forall, x) -> Format.fprintf formatter "A %a" (pp_symbol ark) x
      | (`Exists, x) -> Format.fprintf formatter "E %a" (pp_symbol ark) x
    in
    ArkUtil.pp_print_enum pp_elt formatter (BatList.enum prefix)

  (* Let v be a vertex and let, let r = u_0 ... u_n = v be the path from the
     root to v.  For each i, let (guard_i,move_i) = M(u_i,u_{i+1}).
     path_to_root_formulas computes the list
       [init(x_0); guard_0(x_0) /\ reach(move_0(x_0),x_1);
                   ...;
                   guard_{n-1}(x_{n-1}) /\ reach(move_{n-1}(x_{n-1}),x_{n-1})]
     representing plays of the game where the safety player conforms to the
     path u_0...u_n and the reachability player is constrained to satisfy the
     guards along the path *)
  let path_to_root_formula game_tree vertex =
    let ark = game_tree.ark in
    let rec path_to_root_formula vertex depth =
      match vertex.parent with
      | Some (parent, guard, moves) ->
        let move_map =
          List.fold_left2 (fun map y m ->
              Symbol.Map.add
                y
                (substitute_var_to_level
                   game_tree
                   (depth - 1)
                   m)
                map)
            Symbol.Map.empty
            game_tree.ys
            moves
        in
        (* Map x variables to x_depth and y variables to
           move_depth(x_{depth-1}) *)
        let subst sym =
          if List.mem sym game_tree.xs then
            mk_const ark (var_to_level game_tree depth sym)
          else
            (Symbol.Map.find sym move_map)
        in
        let guard_and_reach =
          mk_and ark [substitute_var_to_level game_tree (depth - 1) guard;
                      substitute_const ark subst game_tree.reach]
        in
        guard_and_reach::(path_to_root_formula parent (depth - 1))
      | None ->
        assert (depth == 0); (* vertex is root *)
        [substitute_var_to_level game_tree depth game_tree.start]
    in
    path_to_root_formula vertex (depth vertex)

  (* Unroll the game k so that the safety player makes k moves (and the
     reachability player makes k-1), using variable indices starting at i *)
  let rec unroll game_tree i k =
    let ark = game_tree.ark in
    if k <= 1 then
      substitute_var_to_level game_tree i game_tree.safe
    else
      (* safe(x_i,y_i) *)
      let safe = substitute_var_to_level game_tree i game_tree.safe in
      (* reach(y_i,x_{i+1}) *)
      let reach =
        let sym_map =
          List.fold_left
            (fun map x ->
               Symbol.Map.add
                 x
                 (mk_const ark (var_to_level game_tree (i + 1) x))
                 map)
            (List.fold_left
               (fun map y ->
                  Symbol.Map.add
                    y
                    (mk_const ark (var_to_level game_tree i y))
                    map)
               Symbol.Map.empty
               game_tree.ys)
            game_tree.xs
        in
        substitute_const ark ((flip Symbol.Map.find) sym_map) game_tree.reach
      in
      let rest = unroll game_tree (i + 1) (k - 1) in
      mk_and ark [safe; mk_implies ark reach rest]

  (* Strengthen the annotation at a vertex with given refinement.  Remove
     coverings that are no longer implied.  The consecution condition is not
     checked. *)
  let strengthen_annotation game_tree vertex refinement =
    let ark = game_tree.ark in
    let new_annotation = mk_and ark [refinement; vertex.annotation] in
    vertex.annotation <- new_annotation;
    let (covers, uncovered) =
      List.partition
        (fun v ->
           Smt.entails ark ~theory:"QF_LRA" v.annotation new_annotation = `Yes)
        vertex.covers
    in
    vertex.covers <- covers;
    uncovered |> List.iter (fun v -> v.state <- Open)

  let rec refine_path_to_root game_tree vertex refine =
    let ark = game_tree.ark in
    let (phi, refine) = match refine with
      | (x::xs) -> (rewrite ark ~down:(nnf_rewriter ark) x, xs)
      | [] -> assert false
    in
    strengthen_annotation game_tree vertex phi;
    match vertex.parent with
    | Some (parent, _, _) ->
      refine_path_to_root game_tree parent refine
    | None ->
      assert (refine = [])

  (*SP: passes chcs to a sygus solver as specification*)
  let simple_tree_interpolant_sygus game_tree root children =
    let ark = game_tree.ark in
    let fo_typ_symbol sym =
      match typ_symbol ark sym with
      | `TyInt -> `TyInt
      | `TyReal -> `TyReal
      | `TyBool -> `TyBool
      | _ -> assert false
    in
    let ftyp = `TyFun (List.map fo_typ_symbol game_tree.xs, `TyBool)
    in
    let relations =
      List.map (fun _ ->
          let sym = mk_symbol ark ftyp in
          sym)
        children
    in
    let allVars = A.make 991 in
    let vars =
      BatList.mapi (fun i sym ->
          let result = mk_var ark i (fo_typ_symbol sym) in
          A.add allVars result;
          result)
        game_tree.xs
    in
    let fresh_var =
      let max_var = ref (List.length vars) in
      Memo.memo (fun sym ->
          incr max_var;
          let result = mk_var ark (!max_var) (fo_typ_symbol sym) in
          A.add allVars result;
          result)          
    in
    let subst phi =
      let assoc = List.combine game_tree.xs vars in
      substitute_const
        ark
        (fun sym ->
          try List.assoc sym assoc
          with Not_found ->
            fresh_var sym)
        phi
    in
    let constraints = A.make 256 in
    List.iter2 (fun child rel ->
        let hypothesis = subst child in
        let conclusion = mk_app ark rel vars in
        A.add constraints (mk_implies ark hypothesis conclusion)
      )
      children
      relations;
    let hypothesis =
      let children = List.map (fun rel -> mk_app ark rel vars) relations in
      mk_and ark ((subst root)::children)
    in
    A.add constraints (mk_implies ark hypothesis (mk_false ark));
    match Sygus.solve_cvc4 ark game_tree.xs (A.to_list allVars) relations (A.to_list constraints) with
    | (`Sat, _) -> `Sat
    | (`Unknown, _) -> `Unknown
    | (`Unsat, specs) ->
       let interp =
         List.map (fun origspec ->
             Log.logf ~level:`always "Original SyGuS solution: %a" (Formula.pp ark) origspec; (* SP *)
             let l = substitute
                       ark
                       (mk_const ark % List.nth game_tree.xs)
                       origspec in
             Log.logf ~level:`always "After substitute solution: %a" (Formula.pp ark) l;
             l)
                  
           specs
       in
       List.iter (fun l -> Log.logf ~level:`always "After substitute solution: %a" (Formula.pp ark) l) interp;
       `Unsat interp

  (* use freqn instead of z3 *)
  let simple_tree_interpolant_freqn game_tree root children =
    let ark = game_tree.ark in
    let module CHC = ArkZ3.CHC in
    let solver = CHC.mk_solver (ArkZ3.mk_context ark []) in
    let fo_typ_symbol sym =
      match typ_symbol ark sym with
      | `TyInt -> `TyInt
      | `TyReal -> `TyReal
      | `TyBool -> `TyBool
      | _ -> assert false
    in
    let relations =
      let typ = `TyFun (List.map fo_typ_symbol game_tree.xs, `TyBool) in
      List.map (fun _ ->
          let sym = mk_symbol ark typ in
          CHC.register_relation solver sym;
          sym)
        children
    in
    let vars =
      BatList.mapi (fun i sym ->
          mk_var ark i (fo_typ_symbol sym))
        game_tree.xs
    in
    let fresh_var =
      let max_var = ref (List.length vars) in
      Memo.memo (fun sym ->
          incr max_var;
          mk_var ark (!max_var) (fo_typ_symbol sym))
    in
    let subst phi =
      let assoc = List.combine game_tree.xs vars in
      substitute_const
        ark
        (fun sym ->
           try List.assoc sym assoc
           with Not_found ->
             fresh_var sym)
        phi
    in
    List.iter2 (fun child rel ->
        let hypothesis = subst child in
        let conclusion = mk_app ark rel vars in
        CHC.add_rule solver hypothesis conclusion)
      children
      relations;
    let hypothesis =
      let children = List.map (fun rel -> mk_app ark rel vars) relations in
      mk_and ark ((subst root)::children)
    in
    CHC.add_rule solver hypothesis (mk_false ark);
    Sygus.solve_freqn ark game_tree.xs (CHC.to_string solver)
    
        (* incr chc_file_number;
         * let chcFileName = "/tmp/chc" ^ string_of_int !chc_file_number ^ ".smt2" in
         * let chcFile = open_out_gen [Open_append; Open_creat] 0o666 chcFileName in
         * let chcFileFormat = BatFormat.formatter_of_out_channel chcFile in
         * Format.fprintf chcFileFormat "\n%s\n" (CHC.to_string solver);
         * close_out chcFile; *)

  let simple_tree_interpolant_chc game_tree root children =
    let ark = game_tree.ark in
    let module CHC = ArkZ3.CHC in
    let solver = CHC.mk_solver (ArkZ3.mk_context ark []) in
    let fo_typ_symbol sym =
      match typ_symbol ark sym with
      | `TyInt -> `TyInt
      | `TyReal -> `TyReal
      | `TyBool -> `TyBool
      | _ -> assert false
    in
    let relations =
      let typ = `TyFun (List.map fo_typ_symbol game_tree.xs, `TyBool) in
      List.map (fun _ ->
          let sym = mk_symbol ark typ in
          CHC.register_relation solver sym;
          sym)
        children
    in
    let vars =
      BatList.mapi (fun i sym ->
          mk_var ark i (fo_typ_symbol sym))
        game_tree.xs
    in
    let fresh_var =
      let max_var = ref (List.length vars) in
      Memo.memo (fun sym ->
          incr max_var;
          mk_var ark (!max_var) (fo_typ_symbol sym))
    in
    let subst phi =
      let assoc = List.combine game_tree.xs vars in
      substitute_const
        ark
        (fun sym ->
           try List.assoc sym assoc
           with Not_found ->
             fresh_var sym)
        phi
    in
    List.iter2 (fun child rel ->
        let hypothesis = subst child in
        let conclusion = mk_app ark rel vars in
        CHC.add_rule solver hypothesis conclusion)
      children
      relations;
    let hypothesis =
      let children = List.map (fun rel -> mk_app ark rel vars) relations in
      mk_and ark ((subst root)::children)
    in
    CHC.add_rule solver hypothesis (mk_false ark);
    CHC.process_rules solver;
    (* incr chc_file_number;
     * let chcFileName = "/tmp/chc" ^ string_of_int !chc_file_number ^ ".smt2" in
     * let chcFile = open_out_gen [Open_append; Open_creat] 0o666 chcFileName in
     * let chcFileFormat = BatFormat.formatter_of_out_channel chcFile in
     * Format.fprintf chcFileFormat "\n%s\n" (CHC.to_string solver);
     * close_out chcFile; *)
    match CHC.check solver [] with
    | `Sat ->
      let interp =
        List.map (fun rel ->
            Log.logf ~level:`always "CHC solution: %a" (Formula.pp ark) (CHC.get_solution solver rel); (* SP *)
            let l = substitute
                      ark
                      (mk_const ark  % List.nth game_tree.xs)
                       (CHC.get_solution solver rel) in
            Log.logf ~level:`always "After substitute solution: %a" (Formula.pp ark) l;
            l)

            (* substitute
             *   ark
             *   (mk_const ark % List.nth game_tree.xs)
             *   (CHC.get_solution solver rel)) *)
        relations
      in
      `Unsat interp
    | `Unsat -> `Sat
    | `Unknown -> `Unknown

  let simple_tree_interpolant game_tree root children =
    let ark = game_tree.ark in
    let smt_ctx = ArkZ3.mk_context ark [] in
    let children = List.map smt_ctx#simplify children in
    let pattern =
      let interp_pattern =
        List.map
          (Z3.Interpolation.mk_interpolant smt_ctx#z3 % smt_ctx#of_formula)
          children
      in
      Z3.Boolean.mk_and smt_ctx#z3 ((smt_ctx#of_formula root)::interp_pattern)
    in
    let params = Z3.Params.mk_params smt_ctx#z3 in
    match Z3.Interpolation.compute_interpolant smt_ctx#z3 pattern params with
    | (_, Some interp, None) ->
      `Unsat (List.map smt_ctx#formula_of interp)
    | (_, None, Some _) -> `Sat
    | (_, _, _) -> `Unknown

  (* Try to find an ancestor of v to cover it. *)
  let find_cover game_tree v =
    let rec find_cover u =
      if Smt.entails game_tree.ark ~theory:"QF_LRA" v.annotation u.annotation = `Yes
      then
        Some u
      else
        match u.parent with
        | Some (u', _, _) -> find_cover u'
        | None -> None
    in
    match v.parent with
    | None -> false
    | Some (parent, _, _) ->
      match find_cover parent with
      | Some cov ->
        logf ~level:`trace "Found cover: %d covers %d" cov.id v.id ;
        v.state <- Covered cov;
        cov.covers <- v::cov.covers;
        true
      | None -> false

  let depth_bounded_safe_region game_tree depth =
    let rec go d vertex =
      if d >= depth then
        []
      else
        match vertex.state with
        | Expanded children ->
          vertex.annotation::(BatList.flatten (List.map (go (d + 1)) children))
        | _ -> [vertex.annotation]
    in
    mk_or (game_tree.ark) (go 0 game_tree.root)

  (* find a vertex at less than a given depth that satisfies a given
     predicate *)
  let find_depth_bounded game_tree depth p =
    let rec go d vertex =
      if d >= depth then
        None
      else
      if p vertex then
        Some vertex
      else
        match vertex.state with
        | Expanded children ->
          List.fold_left (fun found v ->
              match found with
              | Some _ -> found
              | None -> go (d + 1) v)
            None
            children
        | _ -> None
    in
    go 0 game_tree.root

  let force_cover game_tree v =
    let ark = game_tree.ark in
    let path_to_root = path_to_root_formula game_tree v in
    let p2r_formula = mk_and ark path_to_root in
    let v_depth = depth v in
    let p u =
      match u.state with
      | Expanded _ ->
        let u_annotation =
          substitute_var_to_level game_tree v_depth u.annotation
        in
        Smt.entails ark ~theory:"QF_LRA" p2r_formula u_annotation = `Yes
      | _ -> false
    in
    match find_depth_bounded game_tree v_depth p with
    | None -> false
    | Some cov ->
        logf ~level:`trace "Found forced cover: %d may cover %d" cov.id v.id;

        let cov_annotation =
          substitute_var_to_level game_tree v_depth cov.annotation
          |> mk_not ark
        in
        let seq = List.rev (cov_annotation::path_to_root) in
        let smt_ctx = ArkZ3.mk_context ark [] in
        match smt_ctx#interpolate_seq seq with
        | `Sat _ | `Unknown -> assert false
        | `Unsat interpolants ->
          let annotations =
            List.rev_map (substitute_level_to_var game_tree) interpolants
          in
          refine_path_to_root game_tree v annotations;
          if Smt.entails ark ~theory:"QF_LRA" v.annotation cov.annotation = `Yes
          then begin
            logf ~level:`trace "Force cover successful.";
            v.state <- Covered cov;
            cov.covers <- v::cov.covers;
            true
          end else begin
            logf ~level:`trace "Force cover failed.";
            false
          end

  (* Formula representing the safety player's skeleton losing the unrolled
     game.   *)
  let rec losing game_tree skeleton x_map =
    let ark = game_tree.ark in
    match Quantifier.destruct_skeleton_block ark skeleton with
    | `Exists alternatives ->
      List.map (fun (moves, sub_skeleton) ->
          let move_map = (* ys -> moves *)
            List.fold_left2
              (fun map y (_, m) ->
                 let subst v =
                   if Symbol.Map.mem (Hashtbl.find game_tree.level_to_var v) map
                   then
                     Symbol.Map.find (Hashtbl.find game_tree.level_to_var v) map
                   else
                     try
                       Symbol.Map.find
                         (Hashtbl.find game_tree.level_to_var v)
                         x_map
                     with Not_found ->
                       (Log.errorf "moves: %a"
                          (ArkUtil.pp_print_enum (Expr.pp ark))
                          (BatList.enum moves /@ snd);
                        assert false)
                 in
                 Symbol.Map.add y (substitute_const ark subst m) map)
              Symbol.Map.empty
              game_tree.ys
              moves
          in
          (* safe(x_i, moves(x_i)) *)
          let safe =
            let subst v =
              try Symbol.Map.find v move_map
              with Not_found ->
              try Symbol.Map.find v x_map
              with Not_found -> assert false
            in
            substitute_const ark subst game_tree.safe
          in
          match Quantifier.destruct_skeleton_block ark sub_skeleton with
          | `Forall (x_map_list, sub_sub_skeleton) ->
            let x_map =
              List.fold_left2
                (fun map x (_, x') ->
                   Symbol.Map.add x (mk_const ark x') map)
                Symbol.Map.empty
                game_tree.xs
                x_map_list
            in
            (* reach(moves(x_i),x_j) *)
            let reach =
              let subst v =
                try Symbol.Map.find v move_map
                with Not_found ->
                try Symbol.Map.find v x_map
                with Not_found -> assert false
              in
              substitute_const ark subst game_tree.reach
            in
            let losing_subgame = losing game_tree sub_sub_skeleton x_map in
            mk_or ark [mk_not ark safe;
                       mk_and ark (reach::losing_subgame)]
          | `Exists _ -> assert false
          | `Empty -> mk_not ark safe)
        alternatives
    | `Empty -> [mk_false ark]
    | _ -> assert false

  let rec paste game_tree skeleton vertex =
    let ark = game_tree.ark in
    let smt_ctx = ArkZ3.mk_context ark [] in
    let x_map =
      List.fold_left
        (fun map x -> Symbol.Map.add x (mk_const ark x) map)
        Symbol.Map.empty
        game_tree.xs
    in
    if not (force_cover game_tree vertex) then
      let losing_branches = losing game_tree skeleton x_map in

      let vertex_formula =
        match vertex.parent with
        | Some (_, guard, move) ->
          (* guard(x_lev) *)
          let level = (-1) in
          let guard =
            substitute_var_to_level game_tree level guard
          in
          (* y -> move(x_lev) *)
          let move_map = 
            List.fold_left2
              (fun map y m ->
                 Symbol.Map.add
                   y
                   (substitute_var_to_level game_tree level m)
                   map)
              Symbol.Map.empty
              game_tree.ys
              move
          in
          (* reach(move(x_lev),x) *)
          let reach =
            let subst v =
              try Symbol.Map.find v move_map
              with Not_found -> mk_const ark v
            in
            substitute_const ark subst game_tree.reach
          in
          mk_and ark [guard; reach; vertex.annotation]
        | None -> game_tree.start
      in
      let interp =
        simple_tree_interpolant_freqn game_tree vertex_formula losing_branches
      in
      match interp with
      | `Sat -> assert false
      | `Unknown -> assert false
      | `Unsat not_guards ->
        let (guards, alternatives) =
          match Quantifier.destruct_skeleton_block ark skeleton with
          | `Exists alternatives ->
            let true_ = mk_true ark in
            List.fold_right2 (fun not_guard alt (guards, alts) ->
                if Formula.equal true_ not_guard then
                  (guards, alts)
                else
                  let guard =
                    mk_not ark not_guard
                    |> rewrite ark ~down:(nnf_rewriter ark)
                    |> smt_ctx#simplify
                  in
                  logf ~level:`always "Guard: %a" (Formula.pp ark) guard;
                  (guard::guards, alt::alts))
              not_guards
              alternatives
              ([], [])
          | _ -> assert false
        in
        let children =
          List.map2 (fun (moves, sub_skeleton) guard ->
              let moves =
                List.map (substitute_level_to_var game_tree % snd) moves
              in
              let id = game_tree.max_vertex + 1 in
              game_tree.max_vertex <- id;
              { id = id;
                annotation = mk_true ark;
                state = Open;
                covers = [];
                parent = Some (vertex, guard, moves) })
            alternatives
            guards
        in
        vertex.annotation <- mk_or ark guards;
        vertex.state <- Expanded children;
        List.iter2 (fun (_, sub_skeleton) child ->
            match Quantifier.destruct_skeleton_block ark sub_skeleton with
            | `Forall (_, sub_sub_skeleton) ->
              paste game_tree sub_sub_skeleton child
            | `Empty -> ()
            | _ -> assert false)
          alternatives
          children

  let rec expand_vertex game_tree vertex k =
    logf ~level:`always "Expanding vertex #%d with k %d" vertex.id k;
    let ark = game_tree.ark in
    let vertex_depth = depth vertex in
    let rec prefix level =
      if level < vertex_depth then
        let xs =
          List.map
            (fun x -> (`Forall, var_to_level game_tree level x))
            game_tree.xs
        in
        xs@(prefix (level + 1))        
      else if level < vertex_depth + k then
        let xs =
          List.map
            (fun x -> (`Forall, var_to_level game_tree level x))
            game_tree.xs
        in
        let ys =
          List.map
            (fun y -> (`Exists, var_to_level game_tree level y))
            game_tree.ys
        in
        xs@ys@(prefix (level + 1))
      else
        []
    in
    let path_to_root = path_to_root_formula game_tree vertex in
    let unrolled = unroll game_tree vertex_depth k in
    (* logf ~level:`always "path_to_root in expand: %a" (Formula.pp ark) (mk_and ark path_to_root);
     * logf ~level:`always "unrolled in expand: %a" (Formula.pp ark) unrolled; *)
    let game_matrix =
      mk_implies ark (mk_and ark path_to_root) unrolled
      |> rewrite ark ~down:(nnf_rewriter ark)
    in
    let game_prefix = prefix 0 in
    (* logf ~level:`always "game_prefix in expand: %a" (pp_prefix ark) game_prefix; (\* SP *\)
     * logf ~level:`always "game_matrix in expand: %a" (pp_expr_smtlib2 ark) game_matrix; (\* SP *\) *)
    (* let (tmpfn, tmpf) = Filename.open_temp_file "game_paths" ".smt2" in
     * let tmpfo = Format.formatter_of_out_channel tmpf in
     * Format.fprintf tmpfo "(assert %a)\n" (pp_expr_smtlib2 ark) (pp_prefix_matrix ark game_matrix game_prefix);
     * Format.fprintf tmpfo "(check-sat)\n";
     * Pervasives.close_out tmpf; *)
    (* logf ~level:`always "game formula in expand: %a" ; (\* SP *\) *)
    match Quantifier.winning_skeleton ark game_prefix game_matrix with
    | `Sat skeleton ->
      let (x_map, skeleton) =
        let skeleton =
          Quantifier.minimize_skeleton ark skeleton game_matrix
        in
        logf ~level:`always "skeleton in expand: %a" (Quantifier.pp_skeleton ark) skeleton; (* SP *)
        match Quantifier.destruct_skeleton_block ark skeleton with
        | `Forall (block, sub_skeleton) ->
          let x_map =
            List.fold_left
              (fun map x ->
                 Symbol.Map.add x
                   (mk_const ark (var_to_level game_tree vertex_depth x))
                   map)
              Symbol.Map.empty
              game_tree.xs
          in
          (x_map, sub_skeleton)
        | _ -> assert false
      in
      let lost =
        mk_and ark (losing game_tree skeleton x_map)
        |> rewrite ark ~down:(nnf_rewriter ark)
      in
      begin
        let smt_ctx = ArkZ3.mk_context ark [] in
        match smt_ctx#interpolate_seq (List.rev (lost::path_to_root)) with
        | `Sat _ | `Unknown -> assert false
        | `Unsat interpolants ->
          let annotations =
            List.rev_map (substitute_level_to_var game_tree) interpolants
          in
          refine_path_to_root game_tree vertex annotations;
          paste game_tree skeleton vertex;
          true
      end
    | `Unsat skeleton ->
      begin match vertex.parent with
        | None ->
          logf ~level:`trace "Failed to expand.  Reachbility player wins!@\n%a"
            (Quantifier.pp_skeleton ark) skeleton;
          false
        | Some (parent, _, _) ->
          let k' = max (k + 1) (height parent) in
          logf ~level:`always
            "Failed to expand vertex #%d.  Moving up with game depth %d."
            vertex.id
            k';
          delete_children game_tree parent;
          expand_vertex game_tree parent k'
      end
    | `Unknown -> assert false

  let pp formatter game_tree =
    let open Format in
    let ark = game_tree.ark in
    let pp_sep formatter () = Format.fprintf formatter "@;" in
    let rec go formatter v =
      let (guard, moves) = match v.parent with
        | Some (_, guard, moves) -> (guard, moves)
        | _ -> assert false
      in
      fprintf formatter "@[<v 0>when@;  %a@;move (%a)@;  @[<v 0>#%d [%a]@;%a@]@]"
        (Formula.pp ark) guard
        (ArkUtil.pp_print_enum (Expr.pp ark)) (BatList.enum moves)
        v.id
        (Formula.pp ark) v.annotation
        (ArkUtil.pp_print_enum_nobox ~pp_sep go) (BatList.enum (children v));
      match v.state with
      | Covered x -> fprintf formatter "Covered by #%d" x.id
      | _ -> ()
    in
    let root = game_tree.root in
    fprintf formatter "#%d [%a]@;@[<v 0>%a@]"
      root.id
      (Formula.pp ark) root.annotation
      (ArkUtil.pp_print_enum_nobox ~pp_sep go) (BatList.enum (children root))
end

let solve ark (xs, ys) ~start ~safe ~reach =
  let game_tree = GameTree.empty ark (xs, ys) ~start ~safe ~reach in
  let nb_rounds = ref 0 in
  let rec go v =
    incr nb_rounds;
    logf ~level:`always
      "-- Round %d ---------------------------------------------"
      (!nb_rounds);
    logf ~level:`always "%a" GameTree.pp game_tree;

    (*    assert (GameTree.well_labeled game_tree);*)
    if GameTree.expand_vertex game_tree v 1 then
      match GameTree.get_open game_tree with
      | Some u -> go u
      | None -> Some game_tree
    else
      None
  in
  if GameTree.expand_vertex game_tree (GameTree.root game_tree) 1 then
    match GameTree.get_open game_tree with
    | Some u -> go u
    | None -> Some game_tree
  else
    None
