(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *
 * Programmer: Mayer Goldberg, 2021
 *)

 #use "tag-parser.ml";;

 exception X_not_yet_implemented;;
 exception X_this_should_not_happen;;
 
 type var' = 
   | VarFree of string
   | VarParam of string * int
   | VarBound of string * int * int;;
 
 type expr' =
   | ScmConst' of sexpr
   | ScmVar' of var'
   | ScmBox' of var'
   | ScmBoxGet' of var'
   | ScmBoxSet' of var' * expr'
   | ScmIf' of expr' * expr' * expr'
   | ScmSeq' of expr' list
   | ScmSet' of var' * expr'
   | ScmDef' of var' * expr'
   | ScmOr' of expr' list
   | ScmLambdaSimple' of string list * expr'
   | ScmLambdaOpt' of string list * string * expr'
   | ScmApplic' of expr' * (expr' list)
   | ScmApplicTP' of expr' * (expr' list);;
 
 let var_eq v1 v2 =
 match v1, v2 with
   | VarFree (name1), VarFree (name2) -> String.equal name1 name2
   | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
     major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
   | VarParam (name1, index1), VarParam (name2, index2) ->
        index1 = index2 && (String.equal name1 name2)
   | _ -> false
 
 let rec expr'_eq e1 e2 =
   match e1, e2 with
   | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
   | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
   | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                             (expr'_eq dit1 dit2) &&
                                               (expr'_eq dif1 dif2)
   | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
         List.for_all2 expr'_eq exprs1 exprs2
   | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
         (var_eq var1 var2) && (expr'_eq val1 val2)
   | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
      (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
   | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
      (String.equal var1 var2) &&
        (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
   | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
      (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
   | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
       (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
   | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
   | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
   | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
   | _ -> false;;
 
 module type SEMANTIC_ANALYSIS = sig
   val annotate_lexical_addresses : expr -> expr'
   val annotate_tail_calls : expr' -> expr'
   val box_set : expr' -> expr'
   val run_semantics : expr -> expr'
 end;; (* end of module type SEMANTIC_ANALYSIS *)
 
 module Semantic_Analysis = struct
 
   let rec lookup_in_rib name = function
     | [] -> None
     | name' :: rib ->
        if name = name'
        then Some(0)
        else (match (lookup_in_rib name rib) with
              | None -> None
              | Some minor -> Some (minor + 1));;
 
   let rec lookup_in_env name = function
     | [] -> None
     | rib :: env ->
        (match (lookup_in_rib name rib) with
         | None ->
            (match (lookup_in_env name env) with
             | None -> None
             | Some(major, minor) -> Some(major + 1, minor))
         | Some minor -> Some(0, minor));;
 
   let tag_lexical_address_for_var name params env = 
     match (lookup_in_rib name params) with
     | None ->
        (match (lookup_in_env name env) with
         | None -> VarFree name
         | Some(major, minor) -> VarBound(name, major, minor))
     | Some minor -> VarParam(name, minor);;

   (* run this first! *)
   let annotate_lexical_addresses pe = 
    let rec run pe params env = match pe with
      | ScmConst(v) -> ScmConst'(v)
      | ScmVar(v) -> ScmVar'(tag_lexical_address_for_var v params env)
      | ScmIf(test, dit, dif) -> ScmIf'((run test params env), (run dit params env), (run dif params env))
      | ScmSeq(exprs) -> ScmSeq'(List.map (function (x) -> run x params env) exprs)
      | ScmSet(ScmVar(v), expr) -> ScmSet'((tag_lexical_address_for_var v params env), (run expr params env))
      | ScmDef(ScmVar(v), expr) -> ScmDef'((tag_lexical_address_for_var v params env), (run expr params env))
      | ScmOr(exprs) -> ScmOr'(List.map (function (x) -> run x params env) exprs)
      | ScmLambdaSimple(vars, exprs) -> ScmLambdaSimple'(vars, (run exprs vars (List.append [params] env)))
      | ScmLambdaOpt(vars, opt, exprs) -> ScmLambdaOpt'(vars, opt, (run exprs (List.append vars [opt]) (List.append [params] env)))
      | ScmApplic(expr, exprs) -> ScmApplic'((run expr params env), (List.map (function (x) -> run x params env) exprs))
      | _ -> raise X_this_should_not_happen
    in 
    run pe [] [];;
 
   let rec rdc_rac s =
     match s with
     | [e] -> ([], e)
     | e :: s ->
        let (rdc, rac) = rdc_rac s
        in (e :: rdc, rac)
     | _ -> raise X_this_should_not_happen;;
   
   (* run this second! *)
   let annotate_tail_calls pe =
    let rec run pe in_tail = match pe with
      | ScmConst'(_) -> pe
      | ScmVar'(_) -> pe
      | ScmIf'(test, dit, dif) -> ScmIf'((run test false), (run dit in_tail), run dif in_tail)
      | ScmSeq'(exprs) -> let (exceptLast, last) = rdc_rac exprs in
        ScmSeq'(List.rev((run last in_tail)::(List.map (function (exp) -> run exp false) (List.rev(exceptLast)))))
      | ScmSet'(v, expr) -> ScmSet'(v, run expr false)
      | ScmDef'(v, expr) -> ScmDef'(v, run expr in_tail)
      | ScmOr'(exprs) -> let (exceptLast, last) = rdc_rac exprs in
        ScmOr'(List.rev((run last in_tail)::(List.map (function (exp) -> run exp false) (List.rev(exceptLast)))))
      | ScmLambdaSimple'(vars, exprs) -> ScmLambdaSimple'(vars, (run exprs true))
      | ScmLambdaOpt'(vars, opt, exprs) -> ScmLambdaOpt'(vars, opt, (run exprs true))
      | ScmApplic'(expr, exprs) -> (match in_tail with
        | true -> ScmApplicTP' ((run expr false), (List.map (function (e) -> run e false) exprs))
        | false -> ScmApplic'((run expr false), (List.map (function (e) -> run e false) exprs)))
      | _ -> raise X_this_should_not_happen
    in 
    run pe false;;
 
   (* boxing *)
    let rec find_reads name enclosing_lambda expr selector_enclosing = match expr with 
      | ScmVar' (VarFree v) | ScmVar' (VarParam (v,_)) | ScmVar' (VarBound (v,_,_)) when v = name -> [enclosing_lambda]
      | ScmIf' (test, dit, dif) ->
        let test_reads = find_reads name enclosing_lambda test selector_enclosing in
        let dit_reads = find_reads name enclosing_lambda dit selector_enclosing in
        let dif_reads = find_reads name enclosing_lambda dif selector_enclosing in
        List.append (List.append test_reads dit_reads) dif_reads
      | ScmOr'(exprs) -> List.flatten (List.map (function (expr) -> find_reads name enclosing_lambda expr selector_enclosing) exprs)
      | ScmSeq'(exprs) -> List.flatten (List.map (function (expr) -> find_reads name enclosing_lambda expr selector_enclosing) exprs)
      | ScmSet'(_, expr) -> find_reads name enclosing_lambda expr selector_enclosing
      | ScmDef'(_, expr) -> find_reads name enclosing_lambda expr selector_enclosing
      | ScmApplic'(expr, exprs) -> 
        let first_expr = find_reads name enclosing_lambda expr selector_enclosing in
        List.append (first_expr) (List.flatten (List.map (function (e) -> find_reads name enclosing_lambda e selector_enclosing) exprs))
      | ScmApplicTP'(expr, exprs) -> 
        let first_expr = find_reads name enclosing_lambda expr selector_enclosing in
        List.append (first_expr) (List.flatten (List.map (function (e) -> find_reads name enclosing_lambda e selector_enclosing) exprs))
      | ScmLambdaSimple' (vars, body) ->
        let dont_update_enclosing = fun old_enclosing new_enclosing -> old_enclosing in
        if (List.mem name vars) then [] else find_reads name (selector_enclosing enclosing_lambda expr) body dont_update_enclosing
      | ScmLambdaOpt' (vars, opt, body) ->
        let dont_update_enclosing = fun old_enclosing new_enclosing -> old_enclosing in
        if ((List.mem name vars) || (name = opt)) then [] else find_reads name (selector_enclosing enclosing_lambda expr) body dont_update_enclosing
      | _ -> [];;

    let rec find_writes name enclosing_lambda expr selector_enclosing = match expr with
        | ScmIf' (test, dit, dif) ->
          let test_reads = find_writes name enclosing_lambda test selector_enclosing in
          let dit_reads = find_writes name enclosing_lambda dit selector_enclosing in
          let dif_reads = find_writes name enclosing_lambda dif selector_enclosing in
          List.append (List.append test_reads dit_reads) dif_reads
        | ScmOr'(exprs) -> List.flatten (List.map (function (expr) -> find_writes name enclosing_lambda expr selector_enclosing) exprs)
        | ScmSeq'(exprs) -> List.flatten (List.map (function (expr) -> find_writes name enclosing_lambda expr selector_enclosing) exprs)
        | ScmSet'(v, expr) -> (match v with   
          | VarFree va when va = name -> [enclosing_lambda]
          | VarParam (va,_) when va = name -> [enclosing_lambda]
          | VarBound (va,_,_) when va = name -> [enclosing_lambda]
          | _ -> [])
        | ScmDef'(_, expr) -> find_writes name enclosing_lambda expr selector_enclosing
        | ScmApplic'(expr, exprs) -> 
          let first_expr = find_writes name enclosing_lambda expr selector_enclosing in
          List.append (first_expr) (List.flatten (List.map (function (e) -> find_writes name enclosing_lambda e selector_enclosing) exprs))
        | ScmApplicTP'(expr, exprs) -> 
          let first_expr = find_writes name enclosing_lambda expr selector_enclosing in
          List.append (first_expr) (List.flatten (List.map (function (e) -> find_writes name enclosing_lambda e selector_enclosing) exprs))
        | ScmLambdaSimple' (vars, body) ->
          let dont_update_enclosing = fun old_enclosing new_enclosing -> old_enclosing in
          if (List.mem name vars) then [] else find_writes name (selector_enclosing enclosing_lambda expr) body dont_update_enclosing
        | ScmLambdaOpt' (vars, opt, body) ->
          let dont_update_enclosing = fun old_enclosing new_enclosing -> old_enclosing in
          if ((List.mem name vars) || (name = opt)) then [] else find_writes name (selector_enclosing enclosing_lambda expr) body dont_update_enclosing
        | _ -> [];;

let rec box_set expr = boxings [] expr

and boxings vars_for_boxing expr = match expr with
  | ScmIf'(test, dit, dif) -> ScmIf'((boxings vars_for_boxing test), (boxings vars_for_boxing dit), (boxings vars_for_boxing dif))
  | ScmSeq'(exprs) -> ScmSeq'(List.map (boxings vars_for_boxing) exprs)
  | ScmSet'(v, expr) -> ScmSet'(v, (boxings vars_for_boxing expr))
  | ScmDef'(v, expr) -> ScmDef'(v, (boxings vars_for_boxing expr))
  | ScmOr'(exprs) -> ScmOr'(List.map (boxings vars_for_boxing) exprs)
  | ScmLambdaSimple'(vars, exprs) -> ScmLambdaSimple'(vars, boxings [] (List.fold_right (doBoxings vars) (vars_to_box expr) exprs))
  | ScmLambdaOpt'(vars, opt, exprs) -> ScmLambdaOpt'(vars, opt, boxings [] (List.fold_right (doBoxings (List.append vars [opt])) (vars_to_box expr) exprs))
  | ScmApplic'(expr, exprs) -> ScmApplic'((boxings vars_for_boxing expr), (List.map (boxings vars_for_boxing) exprs))
  | ScmApplicTP'(expr, exprs) -> ScmApplicTP'((boxings vars_for_boxing expr), (List.map (boxings vars_for_boxing) exprs))
  | _ -> expr
 
and index_in_list expr = function 
  | hd :: tl -> if expr = hd then 0 else 1 + index_in_list expr tl
  | _ -> -1
   
and vars_to_box exp = match exp with
| ScmLambdaSimple'(vars, body) -> List.filter (function (v) -> check_box_needed exp body v) vars
| ScmLambdaOpt'(vars, opt, body) -> List.filter (function (v) -> check_box_needed exp body v) (List.append vars [opt])
| _ -> []

and doBoxings vars var expr =
    let varIndex = index_in_list var vars in
    let box = (match expr with
      | ScmSeq'(es) -> ScmSeq'(ScmSet'(VarParam(var, varIndex), ScmBox'(VarParam(var, varIndex)))::es)
      | _ -> ScmSeq'(ScmSet'(VarParam(var, varIndex), ScmBox'(VarParam(var, varIndex)))::[expr])) in
    set_and_get var box
 
and set_and_get var expr = match expr with
  | ScmVar'(v) -> (match v with
    | VarParam(v1, minor) -> if (var = v1) then ScmBoxGet'(VarParam(v1, minor)) else expr
    | VarBound(v1, major, minor) -> if (var = v1) then ScmBoxGet'(VarBound(v1, major, minor)) else expr
    | _ -> expr)
  | ScmIf'(test, dit, dif) -> ScmIf'((set_and_get var test), (set_and_get var dit), (set_and_get var dif))
  | ScmSeq'(exprs) -> ScmSeq'(List.map (set_and_get var) exprs)
  | ScmSet'(v, e) -> (match e with
    | ScmBox'(_) -> expr
    | _ -> (match v with
      | VarParam(v1, minor) -> if (var = v1) then ScmBoxSet'(VarParam(v1, minor), (set_and_get var e)) else ScmSet'(v, (set_and_get var e))
      | VarBound(v1, major, minor) -> if (var = v1) then ScmBoxSet'(VarBound(v1, major, minor), (set_and_get var e)) else ScmSet'(v, (set_and_get var e))
      | _ -> ScmSet'(v,(set_and_get var e))))
  | ScmDef'(v, e) -> ScmDef'(v, (set_and_get var e))
  | ScmOr'(exprs) -> ScmOr'(List.map (set_and_get var) exprs)
  | ScmLambdaSimple'(vars, exprs) -> if (List.mem var vars) then boxings [] expr else ScmLambdaSimple'(vars, (set_and_get var exprs))
  | ScmLambdaOpt'(vars, opt, exprs) -> if (List.mem var vars) || (var = opt) then boxings [] expr else ScmLambdaOpt'(vars, opt, (set_and_get var exprs))
  | ScmApplic'(e, exprs) -> ScmApplic'((set_and_get var e), (List.map (set_and_get var) exprs))
  | ScmApplicTP'(e, exprs) -> ScmApplicTP'((set_and_get var e), (List.map (set_and_get var) exprs))
  | ScmBoxSet'(v, e) -> ScmBoxSet'(v, (set_and_get var e))
  | _ -> expr
 
and check_box_needed enclosing_lambda body var = map (find_reads var enclosing_lambda body (fun old_enclosing new_enclosing -> new_enclosing))
                                                     (find_writes var enclosing_lambda body (fun old_enclosing new_enclosing -> new_enclosing))

and map lambdas_reads lambdas_writes = List.mem true (List.map (map2 lambdas_writes) lambdas_reads) 

and map2 lambdas_writes lambda_read = List.mem false (List.map (function (lambda_write) -> map3 lambda_read lambda_write) lambdas_writes)

and map3 lambda_read lambda_write = if (lambda_read == lambda_write) then true else false

   let run_semantics expr =
     box_set
       (annotate_tail_calls
          (annotate_lexical_addresses expr))
 
 end;; (* end of module Semantic_Analysis *)
