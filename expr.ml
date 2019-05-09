(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

 type unop =
    | Negate
  ;;
      
  type binop =
    | Plus
    | Minus
    | Times
    | Equals
    | LessThan
  ;;
  
  type varid = string ;;
    
  type expr =
    | Var of varid                         (* variables *)
    | Num of int                           (* integers *)
    | Bool of bool                        (* booleans *)
    | Unop of unop * expr                  (* unary operators *)
    | Binop of binop * expr * expr         (* binary operators *)
    | Conditional of expr * expr * expr    (* if then else *)
    | Fun of varid * expr                  (* function definitions *)
    | Let of varid * expr * expr           (* local naming *)
    | Letrec of varid * expr * expr        (* recursive local naming *)
    | Raise                                (* exceptions *)
    | Unassigned                           (* (temporarily) unassigned *)
    | App of expr * expr                   (* function applications *)
  ;;
    
  (*......................................................................
    Manipulation of variable names (varids)
   *)
  
  (* varidset -- Sets of varids *)
  module SS = Set.Make (struct
                         type t = varid
                         let compare = String.compare
                       end ) ;;
  
  type varidset = SS.t ;;
  
  (* same_vars :  varidset -> varidset -> bool
     Test to see if two sets of variables have the same elements (for
     testing purposes) *)
  let same_vars : varidset -> varidset -> bool =
    SS.equal;;
  
  (* vars_of_list : string list -> varidset
     Generate a set of variable names from a list of strings (for
     testing purposes) *)
  let vars_of_list : string list -> varidset =
    SS.of_list ;;
    
  (* free_vars : expr -> varidset
     Return a set of the variable names that are free in expression
     exp *)
  let rec free_vars (exp : expr) : varidset =
    match exp with
    | Var a -> SS.singleton a
    | Num a ->  SS.empty
    | Bool a ->  SS.empty
    | Unop (op, ex) -> free_vars ex
    | Binop (op, ex1, ex2) -> SS.union (free_vars ex1) (free_vars ex2)
    | Conditional (ex1, ex2, ex3) -> SS.union (SS.union (free_vars ex1)  (free_vars ex2)) (free_vars ex3)
    | Fun (v, ex) -> SS.remove v (free_vars ex)
    | Let (v, ex1, ex2) -> SS.union (SS.remove v (free_vars ex2)) (free_vars ex1)
    | Letrec (v, ex1, ex2) -> SS.remove v (SS.union (free_vars ex2) (free_vars ex1))
    | Raise -> SS.empty
    | Unassigned -> SS.empty
    | App (ex1, ex2) -> SS.union (free_vars ex1) (free_vars ex2)
  ;;
    
  (* new_varname : unit -> varid
     Return a fresh variable, constructed with a running counter a la
     gensym. Assumes no variable names use the prefix "var". (Otherwise,
     they might accidentally be the same as a generated variable name.) *)
  
  
  
  let new_varname : unit -> varid =
    let num = ref 0 in
    fun () ->
    num := !num + 1; "var " ^ (string_of_int !num) ;;
  
  (*......................................................................
    Substitution 
  
    Substitution of expressions for free occurrences of variables is the
    cornerstone of the substitution model for functional programming
    semantics.
   *)
  
  (* subst : varid -> expr -> expr -> expr
     Substitute repl for free occurrences of var_name in exp *)
  
    
  let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
    match exp with
    | Var a -> if a = var_name then repl else Var a
    | Num a ->  Num a
    | Bool a ->  Bool a
    | Unop (op, ex) -> 
      Unop(op, (subst var_name repl ex))
    | Binop (op, ex1, ex2) ->
      Binop(op, (subst var_name repl ex1), (subst var_name repl ex2))
    | Conditional (ex1, ex2, ex3) -> 
      Conditional((subst var_name repl ex1), (subst var_name repl ex2), 
                  (subst var_name repl ex3))
    | Fun (v, ex) -> 
      if v = var_name then exp 
      else if SS.mem v (free_vars repl) then 
        let newVar = new_varname () in
        Fun(newVar, (subst v (Var(newVar)) (subst var_name repl ex)))
        else Fun(var_name, (subst var_name repl ex))
    | Let (v, ex1, ex2) -> 
      if v = var_name then Let(v, (subst var_name repl ex1), ex2) 
      else if SS.mem v (free_vars repl) then
        let newVar = new_varname () in
        Let(newVar, (subst var_name repl ex1), (subst v (Var newVar) 
           (subst var_name repl ex2)))
      else Let(v, (subst var_name repl ex1), (subst var_name repl ex2))
    | Letrec (v, ex1, ex2) -> 
      if v = var_name then Letrec(v, (subst var_name repl ex1), ex2) 
      else if SS.mem v (free_vars repl) then
        let newVar = new_varname () in
        Letrec(newVar, (subst var_name repl ex1), (subst v (Var newVar) 
              (subst var_name repl ex2)))
      else Letrec(v, (subst var_name repl ex1), (subst var_name repl ex2))
    | Raise -> Raise
    | Unassigned -> Unassigned 
    | App (ex1, ex2) -> App(subst var_name repl ex1, subst var_name repl ex2)
  ;;
  
  (*......................................................................
    String representations of expressions
   *)
     
      
  (* exp_to_concrete_string : expr -> string
     Returns a concrete syntax string representation of the expr *)
  let rec exp_to_concrete_string (exp : expr) : string =
    match exp with
    | Var a -> a
    | Num a ->  string_of_int a
    | Bool a ->  string_of_bool a
    | Unop (op, ex) -> (match op with
                    | Negate -> "(-)"  ^ 
                        (exp_to_concrete_string ex))
    | Binop (op, ex1, ex2) -> 
      let exp1, exp2 = (exp_to_concrete_string ex1), 
                       (exp_to_concrete_string ex2) in 
       (match op with
         | Plus -> exp1 ^ "(+)" ^ exp2
         | Minus -> exp1 ^ "(-)" ^ exp2
         | Times -> exp1 ^ "(*)" ^ exp2
         | Equals -> exp1 ^ "(=)" ^ exp2
         | LessThan -> exp1 ^ "(<)" ^ exp2)
    | Conditional (ex1, ex2, ex3) -> (exp_to_concrete_string ex1) ^ 
                                     (exp_to_concrete_string ex2) ^ 
                                     (exp_to_concrete_string ex3)
    | Fun (v, ex) -> v ^ (exp_to_concrete_string ex)
    | Let (v, ex1, ex2) -> "Let " ^ (exp_to_concrete_string ex1) 
                            ^ " = " ^ (exp_to_concrete_string ex2)
    | Letrec (v, ex1, ex2) -> "Letrec " ^ v ^ "=" ^(exp_to_concrete_string ex1) 
                              ^ " -> " ^ (exp_to_concrete_string ex2)
    | Raise -> "Raise"
    | Unassigned -> "Unnasigned"
    | App (ex1, ex2) -> (exp_to_concrete_string ex1) ^ " " ^ 
                        (exp_to_concrete_string ex2)
  ;;
  
  (* exp_to_abstract_string : expr -> string
     Returns a string representation of the abstract syntax of the expr *)
  let exp_to_abstract_string (exp : expr) : string =
     match exp with
    | Var a -> "Var(" ^ a ^ ")"
    | Num a ->  "Num(" ^ string_of_int a ^ ")"
    | Bool a ->  "Bool(" ^ string_of_bool a ^ ")"
    | Unop (op, ex) -> (match op with
                    | Negate -> "Negate(" ^ (exp_to_concrete_string ex) ^ ")")
    | Binop (op, ex1, ex2) -> let operation = (match op with
                                            | Plus -> "Plus"
                                            | Minus -> "Minus"
                                            | Times -> "Times"
                                            | Equals -> "Equals"
                                            | LessThan -> "LessThan") in
                                            "Binop(" ^ operation ^ ", " ^ (exp_to_concrete_string ex1) ^ ", " ^ (exp_to_concrete_string ex2) ^ ")"
    | Conditional (ex1, ex2, ex3) -> (exp_to_concrete_string ex1) ^ (exp_to_concrete_string ex2) ^ (exp_to_concrete_string ex3)
    | Fun (v, ex) -> "Fun(" ^ v ^ ", " ^ (exp_to_concrete_string ex) ^ ")"
    | Let (v, ex1, ex2) -> "Let(" ^ v ^ ", " ^ (exp_to_concrete_string ex1) ^ ", " ^ (exp_to_concrete_string ex2) ^ ")"
    | Letrec (v, ex1, ex2) -> "Letrec(" ^ v ^ ", " ^ (exp_to_concrete_string ex1) ^ ", " ^ (exp_to_concrete_string ex2) ^ ")"
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (ex1, ex2) -> "App(" ^ (exp_to_concrete_string ex1) ^ ", " ^ (exp_to_concrete_string ex2) ^ ")"
  ;;
  
let unit_test (condition : bool) (msg : string) : unit =
  if condition then
    Printf.printf "%s passed\n" msg
  else
    Printf.printf "%s FAILED\n" msg ;;

let tests () =
  unit_test (free_vars (Conditional((Binop(Equals, Num(3), Num(4)),
                                    Var("v"), Num(3)))) = SS.singleton "v")
            "free_vars conditional";
  unit_test (free_vars (Unop(Negate, Var("v"))) = SS.singleton "v")
            "free_vars negate";
  unit_test (free_vars (Binop(Plus, Num(1), Var("v"))) = SS.singleton "v")
            "free_vars plus";
  unit_test (free_vars (Let("v", Num(2),
                            Binop(Plus, Num(8), Var("v")))) = SS.empty)
            "free_vars Let";;  