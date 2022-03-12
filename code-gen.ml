#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  let rec get_fvars expr = match expr with
    | ScmBoxSet'(_, exp) -> get_fvars exp
    | ScmIf'(testt, dit, dif) -> List.append (get_fvars testt) (List.append (get_fvars dit) (get_fvars dif))
    | ScmSeq'(exprs) -> List.flatten (List.map get_fvars exprs)
    | ScmSet'(_, exp) -> get_fvars exp
    | ScmDef'(VarFree(v), exp) -> List.append [v] (get_fvars exp)
    | ScmOr'(exprs) -> List.flatten (List.map get_fvars exprs)
    | ScmLambdaSimple'(_, exprs)-> get_fvars exprs
    | ScmLambdaOpt'(_, _, exprs) -> get_fvars exprs
    | ScmApplic'(exp, exprs) -> List.append (get_fvars exp) (List.flatten (List.map get_fvars exprs))
    | ScmApplicTP'(exp, exprs) -> List.append (get_fvars exp) (List.flatten (List.map get_fvars exprs))
    | _ -> [];;
  
    let fvars_tbl asts = List.append (["boolean?"; "flonum?"; "rational?"; "pair?"; "null?"; "char?"; "string?"; "procedure?"; "symbol?"; 
                                      "string-length"; "string-ref"; "string-set!"; "make-string"; "symbol->string"; "char->integer";  
                                      "integer->char"; "exact->inexact"; "eq?"; "+";  "*";  "/"; "="; "<"; "numerator"; "denominator"; "gcd"; 
                                      "car"; "cdr"; "set-car!"; "set-cdr!"; "cons"; "apply";]) (List.flatten (List.map (function (ast) ->
                                                                                                                            get_fvars ast) asts));;

  let rec make_fvars_table fvars table i = match fvars with
    | [] -> table
    | _ -> make_fvars_table (List.tl fvars) (List.append table [((List.hd fvars), i)]) (i+1);;
    
  let rec if_fvar_exists fvar tbl = match tbl with
    | [] -> false
    | _ -> if (fvar == (List.hd tbl)) then true
           else (if_fvar_exists fvar (List.tl tbl));;

  let rec reduce_fvars in_fvars out_fvars = match in_fvars with
    | [] -> out_fvars
    | _ -> (if (if_fvar_exists (List.hd in_fvars) out_fvars)
          then (reduce_fvars (List.tl in_fvars) out_fvars)
          else (reduce_fvars (List.tl in_fvars) (List.append out_fvars [(List.hd in_fvars)])));;

  let rec get_lst_of_consts exprs lst = match exprs with
    | [] -> lst
    | _ -> get_lst_of_consts (List.tl exprs) (List.append lst (get_consts_of_expr (List.hd exprs)))

  and get_consts_of_expr expr = match expr with
    | ScmConst'(ScmPair(car,cdr)) -> List.append (List.append (get_consts_of_expr(ScmConst'(car))) (get_consts_of_expr(ScmConst'(cdr)))) [(ScmPair(car, cdr))]
    | ScmConst'(ScmSymbol(s)) -> [ScmString (s) ; ScmSymbol (s)]
    | ScmConst'(ScmVector(exprs)) -> List.append (List.flatten(List.map (function (e) -> get_consts_of_expr(ScmConst'(e))) exprs)) [ScmVector(exprs)]
    | ScmConst'(e) -> [e]
    | ScmBoxSet'(_, exp) -> get_consts_of_expr exp
    | ScmIf'(testt, dit, dif) -> List.append (get_consts_of_expr testt) (List.append (get_consts_of_expr dit) (get_consts_of_expr dif))
    | ScmSeq'(exprs) -> List.flatten (List.map get_consts_of_expr exprs)
    | ScmSet'(_, exp) -> get_consts_of_expr exp
    | ScmDef'(_, exp) -> get_consts_of_expr exp
    | ScmOr'(exprs) -> List.flatten (List.map get_consts_of_expr exprs)
    | ScmLambdaSimple'(_, exprs)-> get_consts_of_expr exprs
    | ScmLambdaOpt'(_, _, exprs) -> get_consts_of_expr exprs
    | ScmApplic'(exp, exprs) -> List.append (get_consts_of_expr exp) (List.flatten (List.map get_consts_of_expr exprs))
    | ScmApplicTP'(exp, exprs) -> List.append (get_consts_of_expr exp) (List.flatten (List.map get_consts_of_expr exprs))
    | _ -> [];;

    let scm_number_eq num1 num2 =
      match num1, num2 with
      | ScmRational(n1, d1), ScmRational(n2, d2) -> n1 = n2 && d1 = d2
      | ScmReal(r1), ScmReal(r2) -> abs_float(r1 -. r2) < 0.001
      | _ , _ -> false;;

    let rec sexpr_eq_new e1 e2 =
      match e1, e2 with
      | (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
      | ScmBoolean(b1), ScmBoolean(b2) -> b1 = b2
      | ScmChar(c1), ScmChar(c2) -> c1 = c2
      | ScmString(s1), ScmString(s2) -> String.equal s1 s2
      | ScmSymbol(sym1), ScmSymbol(sym2) -> String.equal sym1 sym2
      | ScmNumber(num1), ScmNumber(num2) -> scm_number_eq num1 num2
      | ScmVector(sexprs1), ScmVector(sexprs2) -> if (List.length sexprs1 == List.length sexprs2) then
                                                                  List.for_all2 sexpr_eq_new sexprs1 sexprs2 else false
      | ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq_new car1 car2) && (sexpr_eq_new cdr1 cdr2)
      | _, _ -> false;;
      
    let rec find_x_offset x tbl = match tbl with
      | [] -> -1
      | _-> (match (List.hd tbl) with
            | (const, (offset, code)) -> if (sexpr_eq_new const x) == true 
                                                then offset
                                                else (find_x_offset x (List.tl tbl)));;

    let rec find_y_offset y tbl = match tbl with
      | [] -> -1
      | _-> (match (List.hd tbl) with
            | (const, (offset, code)) -> if (sexpr_eq_new const y) == true
                                                  then offset 
                                                  else (find_y_offset y (List.tl tbl)));;

    let rec create_symbol_in_consts_tbl sym offset tbl = match tbl with
      | [] -> (List.append [((ScmString(sym), (offset, "MAKE_LITERAL_STRING \""^sym^"\"")))]
                  [(ScmSymbol(sym), ((String.length sym)+9+offset, "MAKE_LITERAL_SYMBOL(const_tbl+" ^ (string_of_int offset) ^ ")"))])
      | _-> (match (List.hd tbl) with
          | (const, (off, code)) -> if ((sexpr_eq_new const (ScmString(sym))) == true)
                                                then [ScmSymbol(sym), (offset, "MAKE_LITERAL_SYMBOL(const_tbl+" ^ (string_of_int off) ^ ")")]
                                                else create_symbol_in_consts_tbl sym offset (List.tl tbl));;

    let rec make_const_table consts offset tbl = match consts with
      | [] -> tbl
      | _ -> let const = List.hd consts in (match const with
        | ScmVoid -> make_const_table (List.tl consts) (offset+1) (List.append tbl [(const, (offset, "db T_VOID"))])
        | ScmNil -> make_const_table (List.tl consts) (offset+1) (List.append tbl [(const, (offset, "db T_NIL"))])
        | ScmBoolean(false) -> make_const_table (List.tl consts) (offset+2) (List.append tbl [(const, (offset, "db T_BOOL, 0"))])
        | ScmBoolean(true) -> make_const_table (List.tl consts) (offset+2) (List.append tbl [(const, (offset, "db T_BOOL, 1"))])
        | ScmNumber(ScmRational(num1, num2)) -> make_const_table (List.tl consts) (offset+17) (List.append tbl
                                          [(const, (offset, "MAKE_LITERAL_RATIONAL(" ^ (string_of_int num1) ^ "," ^ (string_of_int num2) ^ ")"))])
        | ScmNumber(ScmReal(n)) -> make_const_table (List.tl consts) (offset+9) (List.append tbl [(const, (offset, 
                                                                                  "MAKE_LITERAL_FLOAT(" ^ (string_of_float n) ^ ")"))])
        | ScmChar(ch) -> make_const_table (List.tl consts) (offset+2) (List.append tbl [(const, (offset, 
                                                                                  "MAKE_LITERAL_CHAR(" ^ (string_of_int (Char.code ch)) ^ ")"))])
        | ScmString(x) -> make_const_table (List.tl consts) ((String.length x)+offset+9) (List.append tbl [(const, (offset, 
                                                                                  "MAKE_LITERAL_STRING \""^x^"\""))])
        | ScmSymbol(sym) -> let symbol_in_consts_tbl = create_symbol_in_consts_tbl sym offset tbl in
          (match (List.length symbol_in_consts_tbl) with
            | 1 -> make_const_table (List.tl consts) (offset+9) (List.append tbl symbol_in_consts_tbl)
            | _ -> make_const_table (List.tl consts) ((String.length sym)+9+offset) (List.append tbl symbol_in_consts_tbl))
        | ScmPair(car, cdr) -> make_const_table (List.tl consts) (offset+17) (List.append tbl [(const, (offset, 
                                    "MAKE_LITERAL_PAIR(const_tbl+" ^ (string_of_int (find_x_offset car tbl)) ^
                                                                            ", const_tbl+" ^ (string_of_int (find_y_offset cdr tbl)) ^ ")")) ])
        | ScmVector(exprs) -> let offsets = List.map (function (e) -> find_x_offset e tbl) exprs in
                              let strings = List.map string_of_int offsets in
                              let res = "const_tbl+" ^ String.concat ", const_tbl+" strings in
                              if (exprs=[]) then make_const_table (List.tl consts) ((List.length exprs)*8+offset+9) 
                                                        (List.append tbl [(const, (offset, "MAKE_LITERAL_VECTOR"))]) 
                                            else make_const_table (List.tl consts) ((List.length exprs)*8+offset+9) 
                                                        (List.append tbl [(const, (offset, "MAKE_LITERAL_VECTOR " ^ res ^ ""))]));;
    let rec if_const_exists const tbl = match tbl with
      | [] -> false
      | _ -> if (sexpr_eq_new const (List.hd tbl)) then true
              else (if_const_exists const (List.tl tbl));;

    let rec reduce_consts in_consts out_consts = match in_consts with
      | [] -> out_consts
      | _ -> (if (if_const_exists (List.hd in_consts) out_consts)
            then (reduce_consts (List.tl in_consts) out_consts)
            else (reduce_consts (List.tl in_consts) (List.append out_consts [(List.hd in_consts)])));;

    let rec index_var_in_fvarstbl fvars v = match fvars with
      | [] -> -1
      | _ -> (match (List.hd fvars) with
        | (varr, offset) -> if varr = v then offset else index_var_in_fvarstbl (List.tl fvars) v);;

    let index_to_lable = ref 0;;
    let inc_index = (fun () -> index_to_lable := (!index_to_lable + 1); (string_of_int !index_to_lable));;

  let rec gen_expr consts fvars env e = match e with
    | ScmConst'(const) -> "mov rax, const_tbl+" ^ (string_of_int(find_x_offset const consts)) ^ "\n"
    | ScmVar'(VarParam(_, m)) -> "mov rax, qword [rbp+WORD_SIZE*(4+" ^ (string_of_int m) ^ ")]\n"
    | ScmSet'(VarParam(_, m), exp) -> (gen_expr consts fvars env exp) ^ "\nmov qword [rbp+WORD_SIZE*(4+" ^ (string_of_int m) ^ ")], rax\n
                                                                                                                  mov rax, SOB_VOID_ADDRESS\n"
    | ScmVar'(VarBound(_, maj, min)) -> "mov rax, qword[rbp+WORD_SIZE*2]\nmov rax, qword [rax+WORD_SIZE*" ^ (string_of_int maj) ^ "]\n
                                                                          mov rax, qword [rax+WORD_SIZE*" ^ (string_of_int min) ^ "]\n"
    | ScmSet'(VarBound(v, maj, min), exp) -> (gen_expr consts fvars env exp) ^ "\nmov rbx, qword [rbp+WORD_SIZE*2]\n
                        mov rbx, qword [rbx+WORD_SIZE*"^ (string_of_int maj) ^"]\nmov qword[rbx+WORD_SIZE*"^ (string_of_int min) ^"], rax\n
                                                                                                                    mov rax, SOB_VOID_ADDRESS\n"
    | ScmVar'(VarFree(v)) -> "mov rax, qword [fvar_tbl+WORD_SIZE*"^ (string_of_int (index_var_in_fvarstbl fvars v)) ^"]\n"
    | ScmSet'(VarFree(v), exp) -> (gen_expr consts fvars env exp) ^ "\nmov qword [fvar_tbl+WORD_SIZE*"^ 
                                                    (string_of_int (index_var_in_fvarstbl fvars v)) ^"], rax\n mov rax, SOB_VOID_ADDRESS\n"
    | ScmOr'(exprs) -> let labelOr = "Lexit_Or" ^  inc_index() in (String.concat ("cmp rax, SOB_FALSE_ADDRESS\njne "^ labelOr^ "\n") 
                                                                                  (generator_lst_exprs consts fvars env exprs [])) ^ labelOr ^ ":\n"
    | ScmIf'(testt, dit, dif) -> let lelse = "Lelse" ^ inc_index() in
                                  let lexit = "Lexit_If" ^ inc_index() in
                                  (gen_expr consts fvars env testt) ^ "cmp rax, SOB_FALSE_ADDRESS\nje " ^ lelse ^ "\n" ^
                                  (gen_expr consts fvars env dit) ^ "jmp " ^ lexit ^ "\n" ^ lelse ^ ":\n" ^
                                  (gen_expr consts fvars env dif) ^ lexit ^ ":\n"
    | ScmDef'(VarFree(v), exp) -> (gen_expr consts fvars env exp) ^ "\nmov qword [fvar_tbl+"^ (string_of_int (index_var_in_fvarstbl fvars v)) ^
                "*WORD_SIZE], rax\nmov rax, SOB_VOID_ADDRESS\n"
    | ScmSeq'(exprs) -> (String.concat "\n" (generator_lst_exprs consts fvars env exprs []))
    | ScmBox'(v) -> "push rcx\n" ^ (gen_expr consts fvars env (ScmVar'(v))) ^ "MALLOC rcx, WORD_SIZE\nmov qword [rcx], rax\n
                                                                                                    mov rax, rcx\npop rcx\n"
    | ScmBoxGet'(v) -> (gen_expr consts fvars env (ScmVar'(v))) ^ "mov rax, qword [rax]\n"
    | ScmBoxSet'(v, exp) -> (gen_expr consts fvars env exp) ^ "push rax\n" ^ (gen_expr consts fvars env (ScmVar'(v))) ^ "pop qword [rax]\nmov rax, SOB_VOID_ADDRESS\n"
    | ScmLambdaSimple'(vars, expr) -> let num = inc_index() in generator_lambda consts fvars vars expr (env+1) num 1
    | ScmLambdaOpt'(vars, opt, expr) ->let num = inc_index() in generator_lambda consts fvars vars expr (env+1) num 0
    | ScmApplic'(proc, args) -> (genenerator_applic consts fvars proc args env)
    | ScmApplicTP'(proc, args) -> (generator_applic_tp consts fvars proc args env)
    | _ -> ""

  and generator_lambda consts fvars vars expr env i cmp =
      let str ="mov rbx, " ^ (string_of_int env) ^"\n
                shl rbx, 3\n
                MALLOC rbx, rbx\n"^                     (* malloc place for the extnEnv *)            
                "mov rcx, PARAM_COUNT\n
                add rcx, 1\n
                shl rcx, 3\n
                MALLOC rcx, rcx\n"^                     (* number of paramaters from the old env + magic *)
                "mov qword[rbx], rcx\n"^
                (* copy the parameters from the old env to extnEnv *)
                "mov rdx, 0\n
                ParamsLoop" ^ i ^ ":\n
                cmp rdx, PARAM_COUNT\n
                jg EndParamsLoop" ^ i ^ "\n
                mov rdi, rdx\n
                add rdi, 4\n"^                          (* after the 4 (old,ret,env,num) *)
                "shl rdi, 3\n
                add rdi, rbp\n
                mov rdi, [rdi]\n"^                      (* copy one paramater *)
                "mov [rcx+WORD_SIZE*rdx], rdi\n"^       (* copy to the allocate place that we bring to the extnEnv*)
                "add rdx, 1\n
                jmp ParamsLoop" ^ i ^"\n
                EndParamsLoop" ^ i ^ ":\n"^
                (* copy the old env to the extnEnv *)
                "mov rcx, 0\n
                EnvLoop" ^ i ^ ":\n
                cmp rcx, "^ (string_of_int (env-1)) ^"\n
                je EndEnvLoop" ^ i ^ "\n
                mov rdx, qword [rbp+2*WORD_SIZE]\n"^    (* the lexical env *)
                "mov rdi, rcx\n
                shl rdi, 3\n
                add rdx, rdi\n"^                        (* go to the lexical address and get the objects each 8 bytes *)
                "mov rdx, [rdx]\n
                mov rdi, rcx\n
                add rdi, 1\n
                shl rdi, 3\n
                add rdi, rbx\n"^                        (* copy to the allocate place for extnEnv *)
                "mov [rdi], rdx\n"^                     (* (get up with 8 because of the vector in extnEnv[0]) *)
                "add rcx, 1\n
                jmp EnvLoop" ^ i ^ "\n
                EndEnvLoop" ^ i ^ ":\n"^
                (* make the closure of the lambda *)
                "MAKE_CLOSURE(rax, rbx, Lcode" ^ i ^ ")\n
                jmp Lcont" ^ i ^ "\n
                Lcode" ^ i ^ ":\n
                push rbp\n
                mov rbp, rsp\n" in
      if (cmp == 1) then (str ^ (gen_expr consts fvars env expr) ^ "leave\nret\nLcont" ^ i ^ ":\n") (* lambdaSimple *)
                else (str ^ "ADJUST_STUCK " ^ (string_of_int ((List.length vars)+1)) ^ "\n" ^ (gen_expr consts fvars env expr) ^ 
                                                                                    "leave\nret\nLcont" ^ i ^ ":\n") (* lambdaOpt *)
                
  and genenerator_applic consts fvars proc args env = "push SOB_NIL_ADDRESS\n" ^
                                                      (if args = [] then "\n" else (String.concat "push rax\n" 
                                                      (generator_lst_exprs consts fvars env (List.rev args) [])) ^ 
                                                      "push rax\n")^
                                                      "push " ^ (string_of_int (List.length args))^"\n"^ 
                                                      (gen_expr consts fvars env proc)^ 
                                                      "CLOSURE_ENV rbx, rax\n
                                                      push rbx\n
                                                      CLOSURE_CODE rbx, rax\n
                                                      call rbx\n
                                                      add rsp, WORD_SIZE\n
                                                      pop rbx\n
                                                      lea rsp, [rsp + 8* rbx]\n
                                                      add rsp, WORD_SIZE\n"

  and generator_applic_tp consts fvars proc args env = "push SOB_NIL_ADDRESS\n" ^
                                                       (if args = [] then "\n" else (String.concat "push rax\n"
                                                       (generator_lst_exprs consts fvars env (List.rev args) [])) ^ 
                                                       "push rax\n")^
                                                       "push " ^ 
                                                       (string_of_int (List.length args)) ^ "\n" ^ 
                                                       (gen_expr consts fvars env proc) ^ 
                                                       "CLOSURE_ENV rbx, rax\n
                                                       push rbx\n
                                                       push qword[rbp+WORD_SIZE]\n
                                                       SHIFT_FRAME "^(string_of_int((List.length args)+4))^"\n
                                                       CLOSURE_CODE rbx, rax\njmp rbx\n"

  and generator_lst_exprs consts fvars env exprs genExprs = match (List.length exprs) with
  | 0 -> genExprs
  | _ -> generator_lst_exprs consts fvars env (List.tl exprs) (List.append genExprs [(gen_expr consts fvars env (List.hd exprs))]);;

  let make_consts_tbl asts = let exprs =  (List.append [ScmConst' ScmVoid; ScmConst'(ScmNil); ScmConst'(ScmBoolean (false)); ScmConst'(ScmBoolean (true))] asts) in 
                              make_const_table (reduce_consts (get_lst_of_consts exprs []) []) 0 [];;
  let make_fvars_tbl asts = make_fvars_table (reduce_fvars (fvars_tbl asts) []) [] 0;;
  let generate consts fvars e = gen_expr consts fvars 0 e;;
end;;

