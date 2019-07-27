open MiniCTypes
open EvalUtils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec srl x a = match a with 
| [] -> false  
| (s,v)::t -> if s= x then true else srl x t;; 

let rec eval_expr env e = match e with
| Int (i) -> Int_Val(i)
| Bool (b) -> Bool_Val(b)
| ID (s) -> if srl s env = true then 
let (str,v) =  List.find (fun (string,value) -> if string=s then true else false) env in v 
else raise (DeclareError("id"))

| Add (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val (i),Int_Val (k)) -> Int_Val (i+k)
   | _ -> raise (TypeError("add")) )

| Sub (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val (i),Int_Val (k)) -> Int_Val (i-k)
   | _ -> raise (TypeError("sub")) )

| Mult (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> Int_Val (i*k)
   | _ -> raise (TypeError("mult")) )

| Div (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) ->  
if k = 0 then raise (DivByZeroError)
else Int_Val(i/k)
   | _ -> raise (TypeError("divtype")) )

| Pow (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) ->
Int_Val(int_of_float (float_of_int i**float_of_int k))
   | _ -> raise (TypeError("add")) )

| Or (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Bool_Val(i), Bool_Val(k)) -> Bool_Val (i || k)
   | _ -> raise (TypeError("or")) )

| And (x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Bool_Val(i), Bool_Val(k)) -> Bool_Val (i && k)
   | _ -> raise (TypeError("or")) )

| Not (x) -> (match (eval_expr env x) with
   | Bool_Val(i) -> Bool_Val (not i)
   | _ -> raise (TypeError("or")) )

| Greater(x,y) ->  (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i>k then Bool_Val(true) else Bool_Val(false)
   | _ -> raise (TypeError("greater")) )

| Less(x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i<k then Bool_Val(true) else Bool_Val(false)
   | _ -> raise (TypeError("less")) )

| GreaterEqual(x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i>=k then Bool_Val(true) else Bool_Val(false)
   | _ -> raise (TypeError("greaterE")) )

| LessEqual(x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i <= k then Bool_Val(true) else Bool_Val(false)
   | _ -> raise (TypeError("lessE")) )

| Equal(x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i=k then Bool_Val(true) else Bool_Val(false)
   | (Bool_Val(i), Bool_Val(k)) -> if i=k then Bool_Val(true) else Bool_Val(false)
   | _ -> raise (TypeError("E")) )

| NotEqual(x,y) -> (match (eval_expr env x,eval_expr env y) with
   | (Int_Val(i),Int_Val(k)) -> if i=k then Bool_Val(false) else Bool_Val(true)
   | (Bool_Val(i), Bool_Val(k)) -> if i=k then Bool_Val(false) else Bool_Val(true)
   | _ -> raise (TypeError("NE")) );;

let helptype a = match a with 
| Int_Val(x) -> Int_Type
| Bool_Val(y) -> Bool_Type;;

let helpbool b = match b with 
| Bool_Val(x) -> x;;

let rec eval_stmt env s = match s with 
| NoOp -> env

| Declare(d,str) -> (match d with 
   | Int_Type -> if (srl str env) = true then raise (DeclareError("decl"))
   else (str, Int_Val 0)::env  
   | Bool_Type -> if (srl str env) = true then raise (DeclareError("decl1"))
   else (str, Bool_Val false)::env)

| Assign(str, exp) -> if (srl str env) = false then raise (DeclareError("assign"))
   else List.map (fun a -> let (x,y)= a in
		      if x=str then if helptype(eval_expr env exp) = helptype(y)
		                    then (str,eval_expr env exp)
		                    else raise (TypeError("tyassign"))
		      else a) env

| Seq(x,y) -> eval_stmt (eval_stmt env x) y

| If(g,x,y) -> if helptype(eval_expr env g) != Bool_Type then raise (TypeError("if"))
else (if helpbool(eval_expr env g)= true then eval_stmt env x else eval_stmt env y)

| While(g,y) -> if helptype(eval_expr env g) != Bool_Type then raise (TypeError("while"))
else (if helpbool(eval_expr env g) = true then eval_stmt (eval_stmt env y) s else env)

| DoWhile(y,g) -> if helptype(eval_expr env g) != Bool_Type then raise (TypeError("dowhile"))
else let k =eval_stmt env y in (if helpbool(eval_expr k g) = true then eval_stmt k s else k) 

|Print(exp) -> (match (eval_expr env exp) with 
        | Bool_Val(b) -> let str = (string_of_bool b)^"\n" in let ()= print_output_string str in env 
        | Int_Val(i) -> let str = (string_of_int i)^"\n" in let()=print_output_string str in env )


