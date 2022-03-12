#use "reader.ml";;

type expr =
  | ScmConst of sexpr
  | ScmVar of string
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmSet of expr * expr
  | ScmDef of expr * expr
  | ScmOr of expr list
  | ScmLambdaSimple of string list * expr
  | ScmLambdaOpt of string list * string * expr
  | ScmApplic of expr * (expr list);;

exception X_syntax_error of sexpr * string;;
exception X_reserved_word of string;;
exception X_proper_list_error;;
exception X_not_implemented;;

let rec list_to_proper_list = function
| [] -> ScmNil
| hd::[] -> ScmPair (hd, ScmNil)
| hd::tl -> ScmPair (hd, list_to_proper_list tl);;

let rec list_to_improper_list = function
| [] -> raise X_proper_list_error
| hd::[] -> hd
| hd::tl -> ScmPair (hd, list_to_improper_list tl);;

let rec scm_append scm_list sexpr =
match scm_list with
| ScmNil -> sexpr
| ScmPair (car, cdr) -> ScmPair (car, scm_append cdr sexpr)
| _ -> raise (X_syntax_error (scm_list, "Append expects a proper list"))

let rec scm_map f sexpr =
match sexpr with
| ScmNil -> ScmNil
| ScmPair (car, cdr) -> ScmPair (f car, scm_map f cdr)
| _ -> raise (X_syntax_error (sexpr, "Map expects a list"));;

let rec scm_zip f sexpr1 sexpr2 =
match sexpr1, sexpr2 with
| ScmNil, ScmNil -> ScmNil
| ScmPair (car1, cdr1), ScmPair (car2, cdr2) -> ScmPair (f car1 car2, scm_zip f cdr1 cdr2)
| _, _ ->
    let sepxrs = list_to_proper_list [ScmSymbol "sexpr1:"; sexpr1; ScmSymbol "sexpr2:"; sexpr2] in
    raise (X_syntax_error (sepxrs, "Zip expects 2 lists of equal length"));;

let rec scm_list_to_list = function
| ScmPair (hd, tl) -> hd::(scm_list_to_list tl)
| ScmNil -> []
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_is_list = function
| ScmPair (hd, tl) -> scm_is_list tl
| ScmNil -> true
| _ -> false

let rec scm_list_length = function
| ScmPair (hd, tl) -> 1 + (scm_list_length tl)
| ScmNil -> 0
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec untag expr =
let untag_vars vars = List.map (fun s -> ScmSymbol s) vars in
let untag_tagged_list tag exprs = list_to_proper_list (ScmSymbol tag::(List.map untag exprs)) in

let untag_lambda_opt vars var body =
let vars = match vars with
| [] -> ScmSymbol var
| _ -> list_to_improper_list (untag_vars (vars@[var])) in
list_to_proper_list ([ScmSymbol "lambda"; vars]@body) in

match expr with
| (ScmConst (ScmSymbol(_) as sexpr)
    | ScmConst (ScmNil as sexpr)
    | ScmConst (ScmPair (_, _) as sexpr)) -> list_to_proper_list [ScmSymbol "quote"; sexpr]
| ScmConst s -> s
| ScmVar (name) -> ScmSymbol(name)
| ScmIf (test, dit, ScmConst (ScmVoid)) -> untag_tagged_list "if" [test; dit]
| ScmIf (test, dit, dif) -> untag_tagged_list "if" [test; dit; dif]
| ScmSeq(exprs) -> untag_tagged_list "begin" exprs
| ScmSet (var, value) -> untag_tagged_list "set!" [var; value]
| ScmDef (var, value) -> untag_tagged_list "define" [var; value]
| ScmOr (exprs) -> untag_tagged_list "or" exprs
| ScmLambdaSimple (vars, ScmSeq(body)) ->
    let vars = list_to_proper_list (untag_vars vars) in
    let body = List.map untag body in
    list_to_proper_list ([ScmSymbol "lambda"; vars]@body)
| ScmLambdaSimple (vars, body) ->
    let vars = list_to_proper_list (untag_vars vars) in
    list_to_proper_list ([ScmSymbol "lambda"; vars; untag body])
| ScmLambdaOpt (vars, var, ScmSeq(body)) ->
    let body = List.map untag body in
    untag_lambda_opt vars var body
| ScmLambdaOpt (vars, var, body) ->
    let body = [untag body] in
    untag_lambda_opt vars var body
| ScmApplic(procedure, args) -> list_to_proper_list (List.map untag (procedure::args));;

let rec string_of_expr expr =
string_of_sexpr (untag expr)

let scm_number_eq n1 n2 =
match n1, n2 with
| ScmRational(numerator1, denominator1), ScmRational(numerator2, denominator2) ->
        numerator1 = numerator2 && denominator1 = denominator2
| ScmReal(real1), ScmReal(real2) -> abs_float(real1 -. real2) < 0.001
| _, _ -> false

let rec sexpr_eq s1 s2 =
match s1, s2 with
| (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
| ScmBoolean(bool1), ScmBoolean(bool2) -> bool1 = bool2
| ScmChar(char1), ScmChar(char2) -> char1 = char2
| ScmString(string1), ScmString(string2) -> String.equal string1 string2
| ScmSymbol(symbol1), ScmSymbol(symbol2) -> String.equal symbol1 symbol2
| ScmNumber(number1), ScmNumber(number2) -> scm_number_eq number1 number2
| ScmVector(sexprs1), ScmVector(sexprs2) -> List.for_all2 sexpr_eq sexprs1 sexprs2
| ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
| _, _ -> false

let rec expr_eq e1 e2 =
  match e1, e2 with
  | ScmConst (sexpr1), ScmConst (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar (var1), ScmVar (var2) -> String.equal var1 var2
  | ScmIf (test1, dit1, dif1), ScmIf (test2, dit2, dif2) -> (expr_eq test1 test2) &&
                                            (expr_eq dit1 dit2) &&
                                              (expr_eq dif1 dif2)
  | (ScmSeq(exprs1), ScmSeq(exprs2) | ScmOr (exprs1), ScmOr (exprs2)) ->
        List.for_all2 expr_eq exprs1 exprs2
  | (ScmSet (var1, val1), ScmSet (var2, val2) | ScmDef (var1, val1), ScmDef (var2, val2)) ->
        (expr_eq var1 var2) && (expr_eq val1 val2)
  | ScmLambdaSimple (vars1, body1), ScmLambdaSimple (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmLambdaOpt (vars1, var1, body1), ScmLambdaOpt (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmApplic (e1, args1), ScmApplic (e2, args2) ->
     (expr_eq e1 e2) && (List.for_all2 expr_eq args1 args2)
  | _ -> false;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
end;; 

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;

let rec tag_parse_expression sexpr =
let sexpr = macro_expand sexpr in
match sexpr with 
(* Implement tag parsing here *)
  | ScmBoolean(x) -> ScmConst(ScmBoolean (x))
  | ScmNumber(x) -> ScmConst(ScmNumber(x))
  | ScmChar(x) -> ScmConst(ScmChar(x))
  | ScmString(x) -> ScmConst(ScmString(x))
  | ScmNil -> ScmConst(ScmNil)
  | ScmSymbol(x) -> parse_symbol(x)
  | ScmPair(x, y) -> parse_pair (x, y)
  | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized"))

and macro_expand sexpr =
  match sexpr with
  (* Handle macro expansion patterns here *)
  | ScmPair(ScmSymbol "and", b) -> expand_and_to_if b
  | ScmPair(ScmSymbol "cond", b) -> expand_cond_to_if b
  | ScmPair(ScmSymbol "define", b) -> expand_MIT_define_to_def b
  | ScmPair(ScmSymbol "quasiquote", b) -> expand_quasiquote b
  | ScmPair(ScmSymbol "let", b) -> expand_let b
  | ScmPair(ScmSymbol "let*", b) -> expand_let_kleene b
  | ScmPair(ScmSymbol "letrec", b) -> expand_letrec b
  | _ -> sexpr
  
and parse_symbol sym = if List.mem sym reserved_word_list then (raise (X_reserved_word("Exception: Reserved Word"))) else ScmVar sym

and parse_pair (first, second) = match first with
  | ScmSymbol(x) -> if List.mem x reserved_word_list then parse_reservedWords (x, second) else (parse_applic first second)
  | ScmBoolean(x) -> parse_applic first second
  | ScmNumber(x) -> parse_applic first second
  | ScmChar(x) -> parse_applic first second
  | ScmString (x) -> parse_applic first second
  | ScmPair (x, y) -> parse_applic first second
  | _ -> raise (X_syntax_error (first, "Error"))

and parse_reservedWords (name, rest) = match name with
  | "begin" -> parse_seq rest
  | "define" -> parse_define rest
  | "if" -> parse_if rest
  | "lambda" -> parse_lambda rest
  | "or"-> parse_or rest
  | "quote" -> parse_quote rest
  | "set!" -> parse_set rest
  | _ -> raise (X_reserved_word ("Not Reserved Word"))

and parse_applic first rest = ScmApplic((tag_parse_expression first), (List.map tag_parse_expression (scm_list_to_list rest)))

and parse_if sexpr = match sexpr with
  | ScmPair(test, ScmPair(dit, dif)) -> ScmIf((tag_parse_expression test), (tag_parse_expression dit),
    (match dif with
      | ScmPair(a, b) -> tag_parse_expression a
      | _ -> ScmConst ScmVoid))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and parse_or sexpr = match sexpr with
  | ScmNil -> tag_parse_expression (ScmBoolean false)
  | ScmPair (a, ScmNil) -> tag_parse_expression a
  | ScmPair(ScmSymbol "or", sexprs) -> ScmOr (List.map tag_parse_expression (scm_list_to_list sexprs))
  | ScmPair(a,b) -> ScmOr (tag_parse_expression a :: List.map tag_parse_expression (scm_list_to_list b))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and parse_set sexpr = match sexpr with
  | ScmPair (var, vall) -> (match vall with
    | ScmPair (x, y) -> (match var with 
                          | ScmSymbol(first) -> ScmSet ((tag_parse_expression var), (tag_parse_expression x))
                          | _ -> raise (X_syntax_error (var, "Expected variable on LHS of set!")))
    | _ -> raise (X_syntax_error (vall, "Error")))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and parse_quote sexpr = match sexpr with
  | ScmPair (con, ScmNil) -> ScmConst(con)
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and expand_quasiquote sexpr = match sexpr with
  | ScmPair (expr, ScmNil) -> quasi_nested expr
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and quasi_nested expr = match expr with
  | ScmNil -> ScmNil
  | ScmPair (ScmSymbol("unquote"), ScmPair (ex, ScmNil)) -> ex
  | ScmPair (ScmPair (ScmSymbol("unquote"), ScmPair (ex1, ScmNil)), ex2) -> ScmPair(ScmSymbol "cons", ScmPair(ex1, ScmPair(quasi_nested ex2,ScmNil)))
  | ScmPair (ScmSymbol("unquote-splicing"), ScmPair (ex, ScmNil)) -> ScmPair(ScmSymbol "quote", ScmPair(expr,ScmNil))
  | ScmPair (ScmPair (ScmSymbol("unquote-splicing"), ScmPair (ex1, ScmNil)), ex2) -> ScmPair(ScmSymbol "append", ScmPair(ex1, ScmPair(quasi_nested ex2,ScmNil)))
  | ScmPair (first, rest) -> quasi_paired (first, rest)
  | ScmVector vec -> ScmPair (ScmSymbol "list->vector", ScmPair(quasi_nested(List.fold_right (fun first rest -> ScmPair(first, rest)) vec ScmNil),ScmNil))
  | _ -> ScmPair(ScmSymbol "quote", ScmPair(expr,ScmNil))
 
and quasi_paired (first, rest) = match first with
  | ScmPair (a, b) -> ScmPair (ScmSymbol "cons", ScmPair(quasi_nested first, ScmPair(quasi_nested rest,ScmNil)))
  | _ -> ScmPair (ScmSymbol "cons", ScmPair(ScmPair(ScmSymbol("quote"), ScmPair(first, ScmNil)),ScmPair(quasi_nested rest,ScmNil)))

and expand_and_to_if sexpr = match sexpr with
  | ScmNil -> ScmBoolean true
  | ScmPair (first, rest) -> (match rest with
    | ScmNil -> first
    | ScmPair (a, b) -> ScmPair((ScmSymbol "if"), (ScmPair(first, ScmPair(and_to_if rest, ScmPair(ScmBoolean false, ScmNil)))))
    | _ -> raise (X_syntax_error (rest, "Error")))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and and_to_if expr = match expr with
  | ScmPair (ex, ScmNil) -> ex
  | ScmPair (a, b) -> ScmPair((ScmSymbol "if"), (ScmPair(a, ScmPair(and_to_if b, ScmPair(ScmBoolean false, ScmNil)))))
  | _ -> raise (X_syntax_error (expr, "Error"))

and parse_seq sexpr = match sexpr with
    | ScmNil -> ScmConst ScmVoid
    | ScmPair (expr, ScmNil) -> tag_parse_expression expr
    | ScmPair (a, b) -> ScmSeq (tag_parse_expression a ::  List.map tag_parse_expression (scm_list_to_list b))
    | _ -> raise (X_syntax_error (sexpr, "Error"))

and parse_define sexpr = match sexpr with
  | ScmPair (ScmSymbol(var), vall) -> (match vall with
    | ScmPair (x, y) -> ScmDef ((parse_symbol var), (tag_parse_expression x))
    | _ -> raise (X_syntax_error (vall, "Error")))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and expand_MIT_define_to_def sexpr = match sexpr with
  | ScmPair (ScmPair(ScmSymbol(var), args), exprs) -> ScmPair(ScmSymbol "define", ScmPair(ScmSymbol(var),ScmPair(ScmPair(ScmSymbol "lambda",(ScmPair(args,exprs))),ScmNil)))
  | _ -> ScmPair(ScmSymbol "define", sexpr)

and simpleVars vars lst = match vars with
  | ScmNil -> lst
  | ScmPair (ScmSymbol (var), ScmNil) -> var::lst
  | ScmPair (ScmSymbol (var), rest) -> simpleVars rest (var::lst)
  | _ -> []

and optVarslambda vars lst = match vars with
  | ScmPair (ScmSymbol (first), ScmSymbol(second)) -> first::lst
  | ScmPair (ScmSymbol (first), rest) -> optVarslambda rest (first::lst)
  | _ -> []

and optVarLambda = function
  | ScmPair (ScmSymbol (first), ScmSymbol(opt)) -> opt
  | ScmPair (ScmSymbol (first), rest) -> optVarLambda rest
  | _ -> ""

and parse_lambda sexpr = match sexpr with
  | ScmPair (vars, exprs) ->
    if ((scm_is_list vars) == true) then (ScmLambdaSimple ((List.rev (simpleVars vars [])), parse_seq exprs))
      else (match vars with
        | ScmSymbol(var) -> ScmLambdaOpt ([], var, parse_seq exprs)
        | ScmPair (x, y) -> ScmLambdaOpt ((List.rev (optVarslambda vars [])), (optVarLambda vars), parse_seq exprs)
        | _ -> raise (X_syntax_error (sexpr, "Error"))
      )
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and expand_cond_to_if sexpr = match sexpr with
  | ScmPair (expr, ScmNil) -> (match expr with
    | ScmPair (ScmSymbol("else"), rest) -> ScmPair(ScmSymbol "begin",rest)
    | ScmPair (a, ScmPair(ScmSymbol("=>"), b)) -> macro_expand(cond_f_sexpr a b ScmNil)
    | ScmPair (a, b) -> ScmPair((ScmSymbol "if"), (ScmPair(a, ScmPair(ScmPair(ScmSymbol "begin",b), ScmNil))))
    | _ -> raise (X_syntax_error (expr, "Error")))
  | ScmPair (first, rest) -> (match first with
    | ScmPair (ScmSymbol("else"), z)-> ScmPair(ScmSymbol "begin",z)
    | ScmPair (a, ScmPair(ScmSymbol("=>"), b)) -> macro_expand(cond_f_sexpr a b rest)
    | ScmPair (a, b) -> ScmPair((ScmSymbol "if"), (ScmPair(a, ScmPair(ScmPair(ScmSymbol "begin",b), ScmPair(expand_cond_to_if rest, ScmNil)))))
    | _ -> raise (X_syntax_error (first, "Error")))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and cond_f_sexpr expr f rest = match rest with
  | ScmNil -> ScmPair (ScmSymbol "let",
                        ScmPair (ScmPair (ScmPair (ScmSymbol "value", ScmPair (expr, ScmNil)),
                        ScmPair (ScmPair (ScmSymbol "f",
                        ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmNil, f)), ScmNil)), ScmNil)),
                              ScmPair(ScmPair (ScmSymbol "if",
                              ScmPair (ScmSymbol "value",
                              ScmPair (ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmPair (ScmSymbol "value", ScmNil)), ScmNil))), ScmNil)))
  | _ -> ScmPair (ScmSymbol "let",
                        ScmPair (ScmPair (ScmPair (ScmSymbol "value", ScmPair (expr, ScmNil)),
                        ScmPair (ScmPair (ScmSymbol "f",
                        ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmNil, f)), ScmNil)),
                        ScmPair (ScmPair (ScmSymbol "rest",
                        ScmPair (ScmPair (ScmSymbol "lambda", ScmPair (ScmNil, ScmPair (ScmPair(ScmSymbol "cond", rest), ScmNil))), ScmNil)), ScmNil))),
                              ScmPair(ScmPair (ScmSymbol "if",
                              ScmPair (ScmSymbol "value",
                              ScmPair (ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmPair (ScmSymbol "value", ScmNil)),
                              ScmPair (ScmPair (ScmSymbol "rest", ScmNil), ScmNil)))), ScmNil)))
         
and expand_let sexpr = match sexpr with
  | ScmPair (vars, body) -> ScmPair (ScmPair (ScmSymbol "lambda", (ScmPair (list_to_proper_list (List.rev (scm_list_to_list(get_args vars ScmNil))), body))), (list_to_proper_list (List.rev (scm_list_to_list(get_values vars ScmNil)))))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and get_args params cdr = match params with
  | ScmNil -> cdr
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), ScmNil) -> (ScmPair(args,cdr))
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), rest) -> get_args rest (ScmPair(args,cdr))
  | _ -> raise (X_syntax_error (params, "Error"))

and get_values params cdr = match params with
  | ScmNil -> cdr
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), ScmNil)-> (ScmPair(vals,cdr))
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), rest) -> get_values rest (ScmPair(vals, cdr))
  | _ -> raise (X_syntax_error (params, "Error"))
  
and expand_let_kleene sexpr = match sexpr with
  | ScmPair (first, ScmPair(second, ScmNil)) -> (match first with
    | ScmNil -> expand_let sexpr
    | ScmPair (expr, ScmNil) -> expand_let sexpr
    | ScmPair (ScmPair(args, ScmPair(vals, sexp)), rest) -> expand_let (ScmPair (ScmPair (ScmPair (args, ScmPair (vals, sexp)), ScmNil), ScmPair ((expand_let_kleene (ScmPair (rest, ScmPair (second, ScmNil)))), ScmNil))) 
    | _ -> raise (X_syntax_error (first, "Error")))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and expand_letrec sexpr = match sexpr with
  | ScmPair (first, second) -> expand_let (ScmPair((get_rib first ScmNil), set_rib first second))
  | _ -> raise (X_syntax_error (sexpr, "Error"))

and get_rib params cdr = match params with
  | ScmNil -> cdr
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), ScmNil) -> (ScmPair (ScmPair (args, (ScmPair (ScmPair(ScmSymbol ("quote"), ScmPair (ScmSymbol ("whatever"), ScmNil)), ScmNil ))), cdr))
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), rest) -> get_rib rest (ScmPair (ScmPair (args, (ScmPair (ScmPair(ScmSymbol ("quote"), ScmPair (ScmSymbol ("whatever"), ScmNil)), ScmNil ))), cdr))
  | _ -> raise (X_syntax_error (params, "Error"))

and set_rib params cdr = match params with
  | ScmNil -> cdr
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), ScmNil)-> ScmPair (ScmPair (ScmSymbol("set!"), ScmPair (args, ScmPair(vals, sexp))), cdr)
  | ScmPair (ScmPair(args, ScmPair(vals, sexp)), rest) -> set_rib rest (ScmPair (ScmPair (ScmSymbol("set!"), ScmPair (args, ScmPair(vals, sexp))), cdr))
  | _ -> raise (X_syntax_error (params, "Error"))

end;;