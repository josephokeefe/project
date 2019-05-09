open Expr;;
open Evaluation;;

let unit_test (condition : bool) (msg : string) : unit =
    if condition then
      Printf.printf "%s passed\n" msg
    else
      Printf.printf "%s FAILED\n" msg ;;


let tests () =
    unit_test (eval_s (Num(4)) (Env.create ()) = Env.Val (Num(4))) "eval_s num";
    unit_test (eval_d (Num(4)) (Env.create ()) = Env.Val (Num(4))) "eval_d num";
    unit_test (eval_l (Num(4)) (Env.create ()) = Env.Val (Num(4))) "eval_l num";
    unit_test (eval_s (Binop(LessThan, Num(2), Binop(Plus, Num(2), Num(2)))) (Env.create ()) = Env.Val (Bool(true))) "eval_s binop, comparison, bool";
    unit_test (eval_d (Binop(LessThan, Num(2), Binop(Plus, Num(2), Num(2)))) (Env.create ()) = Env.Val (Bool(true))) "eval_d binop, comparison, bool";
    unit_test (eval_l (Binop(LessThan, Num(2), Binop(Plus, Num(2), Num(2)))) (Env.create ()) = Env.Val (Bool(true))) "eval_l binop, comparison, bool";;

tests () ;;