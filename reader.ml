(* reader.ml
 * A skeleton for the reader for the 2021-2022 course on compiler-construction
 *)

 #use "pc.ml";;

 let rec gcd a b =
   match (a, b) with
   | (0, b) -> b
   | (a, 0) -> a
   | (a, b) -> gcd b (a mod b);;
 
 type scm_number =
   | ScmRational of (int * int)
   | ScmReal of float;;
 
 type sexpr =
   | ScmVoid
   | ScmNil
   | ScmBoolean of bool
   | ScmChar of char
   | ScmString of string
   | ScmSymbol of string
   | ScmNumber of scm_number
   | ScmVector of (sexpr list)
   | ScmPair of (sexpr * sexpr);;
 
 type stringInterpolated = 
   | Static of string
   | Dynamic of sexpr;;
 
 module type READER = sig
   val nt_sexpr : sexpr PC.parser
 end;; (* end of READER signature *)
 
 module Reader : READER = struct 
   open PC;;
 
   let unitify nt = pack nt (fun _ -> ());;
 
   let maybe_func nt none_val = 
     pack (maybe nt) (function 
         | None -> none_val
         | Some f -> f);;
 
   let nt_char_0_9 = const (fun ch -> '0' <= ch && ch <= '9') ;;
   let nt_digit_0_9 = pack nt_char_0_9 (fun ch -> (int_of_char ch) - 48);;
   let nt_a_to_z = const (fun ch -> 'a' <= ch && ch <= 'z') ;;
   let nt_A_to_Z = const (fun ch -> 'A' <= ch && ch <= 'Z') ;;
   let nt_hex_options = star (disj_list [nt_char_0_9 ; nt_a_to_z ; nt_A_to_Z]);;
   let nt_chars = const (fun ch -> (ch = '!' || ch = '$' || ch = '^' || ch = '*' || ch == '-' || ch = '_' || ch = '=' ||
                                   ch == '+' || ch = '<' || ch = '>' || ch = '?' || ch = ':' || ch = '/')) ;;
   let nt_symbol_char = disj_list [nt_char_0_9 ; nt_a_to_z ; nt_A_to_Z ; nt_chars] ;;
 
   (* whitespaces *)
   let rec nt_whitespace str =
     const (fun ch -> ch <= ' ') str
   and nt_end_of_line_or_file str =
     let nt1 = unitify (char '\n') in
     let nt2 = unitify nt_end_of_input in
     let nt1 = disj nt1 nt2 in
     nt1 str
   (* comments *)
   and nt_line_comment str =
     let allExceptEnd = star (diff nt_any nt_end_of_line_or_file) in 
     let nt_line = unitify(caten (char ';')(caten allExceptEnd nt_end_of_line_or_file)) in
     nt_line str
 
   and nt_sexprComment str = unitify (caten (word "#;") nt_sexpr) str
   and nt_paired_comment str = 
     let nt1 = disj_list [unitify nt_char; unitify nt_string; unitify nt_comment] in
     let nt_brackets = disj nt1 (unitify (one_of"{}")) in
     let ntExceptBrackets = diff nt_any nt_brackets in
     let ntExceptBrackets = star (disj (unitify ntExceptBrackets) nt1) in
     let nt1 = unitify (caten (char '{') (caten ntExceptBrackets (char '}'))) in
     nt1 str
   and nt_comment str = disj_list [nt_line_comment; nt_sexprComment; nt_paired_comment] str
   and nt_skip_star str =
     let nt1 = disj (unitify nt_whitespace) nt_comment in
     let nt1 = unitify (star nt1) in
     nt1 str
   and make_skipped_star (nt : 'a parser) =
     let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
     let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
     nt1
   (* number *)
   and nt_is_positive str =
     let nt1 = pack (char '+') (fun _ -> true) in
     let ntMinus = pack (char '-') (fun _ -> false) in
     let nt1 = disj nt1 ntMinus in
     let nt1 = maybe_func nt1 true in
     nt1 str
   and nt_natural str = pack (plus (nt_digit_0_9)) (fun digits -> List.fold_left
                                                                 (fun a b -> 10*a+b)
                                                                 0
                                                                 digits) str
   and nt_int str = pack (caten nt_is_positive nt_natural) (fun (is_positive, n) ->
                        if is_positive then n else (- n)) str
 
   and nt_digit_0 str = pack (char '0') (fun ch -> (int_of_char ch) - 48) str
   and nt_fraction str = caten (caten nt_int (char '/')) (diff nt_natural nt_digit_0) str
   and resultFrac ((e, es), ess) = ScmRational (e, ess)
   and gcdFraction ((e, es), ess) = ((e/(abs (gcd e ess)), es), ess/(abs (gcd e ess)))
   and nt_frac str = pack (pack nt_fraction gcdFraction) resultFrac str
   and nt_integer_part str = pack (plus nt_digit_0_9) (fun digits -> List.fold_left
                                                                    (fun num digit -> 10.0 *. num +. (float_of_int digit))
                                                                    0.0
                                                                    digits) str
   and nt_mantissa str = pack (plus nt_digit_0_9) (fun digits ->
                             List.fold_right
                               (fun digit num -> ((float_of_int digit) +. num) /. 10.0) 
                               digits
                               0.0) str
   and nt_exponent str =
     let nt_e = unitify (char_ci 'e') in
     let nt_exp = disj (unitify (word "**")) (unitify (char '^')) in
     let nt_expp = unitify (caten (word "*10") nt_exp) in
     let nt_prefix = disj nt_e nt_expp in
     let nt1 = caten nt_prefix nt_int in
     let nt1 = pack nt1 (fun (_, n) -> Float.pow 10. (float_of_int n)) in
     nt1 str
   and nt_float str =
     let nt_mant = maybe_func nt_mantissa 0.0 in 
     let nt_exp = maybe_func nt_exponent 1.0 in
     let ntA = caten nt_integer_part (caten (char '.') (caten nt_mant nt_exp)) in 
     let ntA = pack ntA (fun (intP, (_, (mant, exp)))-> (intP +. mant)*. exp) in 
     let nt_mant = nt_mantissa in 
     let nt_exp = maybe_func nt_exponent 1.0 in 
     let ntB = caten (char '.') (caten nt_mant nt_exp) in 
     let ntB = pack ntB (fun (_, (mant, exp)) -> mant *. exp) in 
     let ntC = caten nt_integer_part nt_exponent in
     let ntC = pack ntC (fun (intP, exp) -> intP *. exp) in 
     let ntA = disj ntA (disj ntB ntC) in
     let nt = caten nt_is_positive ntA in
     let nt = pack nt (fun (is_positive, x)-> if is_positive then x else -. x) in
     let nt = pack nt (fun x -> ScmReal(x)) in
     nt str
   and nt_number str = 
     let ntIntP = pack nt_int (fun n -> ScmRational(n, 1)) in
     let nt = disj nt_float (disj nt_frac ntIntP) in
     let nt = pack nt (fun r -> ScmNumber r) in
     let nt = not_followed_by nt nt_symbol_char in
     nt str
   (* boolean *)
   and nt_boolean str = 
     let ntF = pack (word_ci "#f") (fun _ -> false) in
     let ntT = pack (word_ci "#t") (fun _ -> true) in 
     let nt = pack (disj ntF ntT) (fun b -> ScmBoolean b) in 
     nt str
   (* symbols *)
   and nt_symbol str =
     let nt = pack (plus nt_symbol_char) list_to_string in
     let nt = pack nt (fun name -> ScmSymbol name) in
     let nt = diff nt nt_number in
     nt str
   (* char *)
   and nt_char_named str =
     let nt1 = disj_list [(make_named_char "newline" '\n');
                          (make_named_char "page" '\012');
                          (make_named_char "return" '\r');
                          (make_named_char "space" ' ');
                          (make_named_char "nul" '\000');
                          (make_named_char "tab" '\t')] in
     nt1 str
   and make_named_char char_name ch = pack (word_ci char_name) (fun _ -> (ScmChar ch))
   and nt_char_simple str = const (fun ch -> ch > ' ') str
   and nt_visible_simple_char str = pack nt_char_simple (fun ch -> ScmChar ch) str
   and nt_char_hex str = pack (caten (char_ci 'x') nt_hex_options) (fun (e, es) -> ScmChar (char_of_int (int_of_string ("0x"^list_to_string(es))))) str
   and nt_char str = pack (caten (word "#\\") (disj_list [nt_char_hex; nt_char_named; nt_visible_simple_char])) (fun (e, es) -> es) str
   
   (* string *)
   and nt_string_literal_char str = 
     let ntALLExcept = disj_list [unitify (char '"'); unitify (char '\\'); unitify (char '~'); unitify nt_string_inter] in
     let ntALLExcept = diff nt_any ntALLExcept in
     let nt = pack ntALLExcept (fun ch -> String.make 1 ch) in
     let nt = pack nt (fun str -> Static str) in
     nt str
   and make_named_meta char_name ch = pack (word_ci char_name) (fun _ -> Static ch)
   and nt_meta str =
     let nt = disj_list [(make_named_meta "\\n" "\n");
                         (make_named_meta "\\f" "\012");
                         (make_named_meta "\\r" "\r");
                         (make_named_meta "\\\\" "\092");
                         (make_named_meta "\\t" "\t");
                         (make_named_meta "\\\"" "\034");
                         (make_named_meta "~~" "~")] in
     nt str
   and nt_string_hex str = pack (caten (word_ci "\\x") (caten nt_hex_options (char ';'))) (fun (e, (es, ess)) -> Static( String.make 1 
                                                                                                                        (char_of_int (int_of_string ("0x"^list_to_string(es)))))) str
   and nt_string_inter str = 
     let nt = caten (word "~{") (caten nt_sexpr (char '}')) in
     let nt = pack nt (fun (_,(sexpr,_))->sexpr) in
     let nt = pack nt (fun sexpr -> Dynamic sexpr) in
     nt str 
 
   and nt_string_part str = star(disj_list [nt_string_literal_char; nt_meta; nt_string_hex; nt_string_inter]) str
 
   and stringify sexpr =
     ScmPair(ScmSymbol "format",
             ScmPair(ScmString "~a",
                     ScmPair(sexpr, ScmNil)))
 
   and nt_string str = 
     let nt = caten (char '"') (caten nt_string_part (char '"')) in
     let nt = pack nt (fun (_,(parts,_)) -> parts) in
     let nt = pack nt
                (fun parts ->
                  let parts = 
                    List.fold_right
                      (fun part rest ->
                        match part, rest with
                        | Static str, ScmNil ->
                           ScmPair(ScmString str, ScmNil)
                        | Dynamic sexpr, ScmNil ->
                           ScmPair(stringify sexpr, ScmNil)
                        | Static str, ScmPair(ScmString str', rest) ->
                           ScmPair(ScmString(str ^ str'), rest)
                        | Static str, rest ->
                           ScmPair(ScmString str, rest)
                        | Dynamic sexpr, rest ->
                           ScmPair(stringify sexpr, rest)
                      )
                      parts
                      ScmNil in
                  match parts with
                  | ScmNil -> ScmString ""
                  | ScmPair(ScmString str, ScmNil) -> ScmString str
                  | ScmPair((ScmPair _) as one, ScmNil) -> one
                  | parts -> ScmPair(ScmSymbol "string-append", parts)) in
     nt str
   (* vector *)
   and nt_vector str =
     let nt1 = word "#(" in
     let nt2 = caten nt_skip_star (char ')') in
     let nt2 = pack nt2 (fun _ -> ScmVector []) in
     let nt3 = plus nt_sexpr in
     let nt4 = char ')' in
     let nt3 = caten nt3 nt4 in
     let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
     let nt2 = disj nt2 nt3 in
     let nt1 = caten nt1 nt2 in
     let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
     nt1 str
   (* list *)
   and nt_list str = 
     let pairProper = caten (char '(') (caten (star nt_sexpr) (char ')') ) in
     let nt_list_proper = pack pairProper (fun (e1, (e2, e3)) -> (List.fold_right 
                                                                 (fun a b -> ScmPair (a, b)) 
                                                                 e2 
                                                                 ScmNil))in
     let pairImproper = caten (char '(')  (caten (plus nt_sexpr) (caten (char '.') (caten nt_sexpr (char ')') ))) in
     let nt_list_improper = pack pairImproper (fun (e1, (e2, (e3, (e4, e5)))) -> (List.fold_right
                                                                                 (fun a b -> ScmPair (a, b)) 
                                                                                 e2 
                                                                                 e4))in
     let nt_list_spaces = char '(' in
     let nt_spaces = pack(caten nt_skip_star (char ')')) (fun _ ->ScmNil) in
     let nt_list_spaces = pack(caten nt_list_spaces nt_spaces)  (fun (_,s)->s)in
     let nt = disj_list [nt_list_proper; nt_list_improper; nt_list_spaces] in
     nt str
   (* quoted forms *)
   and nt_quoted_forms str = 
     let quoted_ = word "'" in 
     let nt_quoted = pack (caten quoted_ nt_sexpr) (fun (e, es) -> ScmPair (ScmSymbol ("quote"), ScmPair (es, ScmNil)))in
     (* QQuoted *)
     let qquoted_ = const (fun ch -> ch == '`')in 
     let nt_qquoted = pack (caten qquoted_ nt_sexpr) (fun (e, es) -> ScmPair (ScmSymbol ("quasiquote"), ScmPair (es, ScmNil)))in
     (* Unquoted *)
     let unquoted_ = const (fun ch -> ch == ',')in
     let nt_unquoted = pack (caten unquoted_ nt_sexpr) (fun (e, es) -> ScmPair (ScmSymbol ("unquote"), ScmPair (es, ScmNil)))in
     (* UnquotedSpliced *) 
     let unquoteSpliced_ = word ",@" in
     let nt_unquoteSpliced = pack (caten unquoteSpliced_ nt_sexpr) (fun (e, es) -> ScmPair (ScmSymbol ("unquote-splicing"), ScmPair (es, ScmNil)))in
     (* Quote-like forms *)
     let nt_quoteLike = disj_list [nt_quoted; nt_qquoted; nt_unquoted; nt_unquoteSpliced] in 
     nt_quoteLike str
   (* sexpr *)
   and nt_sexpr str =
     let nt1 = disj_list [nt_number; nt_boolean; nt_char; nt_symbol; nt_vector; nt_list; nt_string; nt_quoted_forms] in
     let nt1 = make_skipped_star nt1 in
     nt1 str;;
 end;;  (* end of struct Reader *)
 let rec string_of_sexpr = function
   | ScmVoid -> "#<void>"
   | ScmNil -> "()"
   | ScmBoolean(false) -> "#f"
   | ScmBoolean(true) -> "#t"
   | ScmChar('\n') -> "#\\newline"
   | ScmChar('\r') -> "#\\return"
   | ScmChar('\012') -> "#\\page"
   | ScmChar('\t') -> "#\\tab"
   | ScmChar(' ') -> "#\\space"
   | ScmChar(ch) ->
      if (ch < ' ')
      then let n = int_of_char ch in
           Printf.sprintf "#\\x%x" n
      else Printf.sprintf "#\\%c" ch
   | ScmString(str) ->
      Printf.sprintf "\"%s\""
        (String.concat ""
           (List.map
              (function
               | '\n' -> "\\n"
               | '\012' -> "\\f"
               | '\r' -> "\\r"
               | '\t' -> "\\t"
               | ch ->
                  if (ch < ' ')
                  then Printf.sprintf "\\x%x;" (int_of_char ch)
                  else Printf.sprintf "%c" ch)
              (string_to_list str)))
   | ScmSymbol(sym) -> sym
   | ScmNumber(ScmRational(0, _)) -> "0"
   | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
   | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
   | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
   | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
   | ScmVector(sexprs) ->
      let strings = List.map string_of_sexpr sexprs in
      let inner_string = String.concat " " strings in
      Printf.sprintf "#(%s)" inner_string
   | ScmPair(ScmSymbol "quote",
             ScmPair(sexpr, ScmNil)) ->
      Printf.sprintf "'%s" (string_of_sexpr sexpr)
   | ScmPair(ScmSymbol "quasiquote",
             ScmPair(sexpr, ScmNil)) ->
      Printf.sprintf "`%s" (string_of_sexpr sexpr)
   | ScmPair(ScmSymbol "unquote",
             ScmPair(sexpr, ScmNil)) ->
      Printf.sprintf ",%s" (string_of_sexpr sexpr)
   | ScmPair(ScmSymbol "unquote-splicing",
             ScmPair(sexpr, ScmNil)) ->
      Printf.sprintf ",@%s" (string_of_sexpr sexpr)
   | ScmPair(car, cdr) ->
      string_of_sexpr' (string_of_sexpr car) cdr
 and string_of_sexpr' car_string = function
   | ScmNil -> Printf.sprintf "(%s)" car_string
   | ScmPair(cadr, cddr) ->
      let new_car_string =
        Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
      string_of_sexpr' new_car_string cddr
   | cdr ->
      let cdr_string = (string_of_sexpr cdr) in
      Printf.sprintf "(%s . %s)" car_string cdr_string;;