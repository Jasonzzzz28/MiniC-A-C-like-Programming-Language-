open SmallCTypes
open Utils
open TokenTypes

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

    (* start your code here *)


let lookahead toks = match toks with 
h::t -> h 
| _ -> raise (Failure("Empty input to lookahead")) ;;


let rec parse_expr toks = 
parse1 toks 

and parse1 toks =
let (t,e2) = parse2 toks in
match lookahead t with
| Tok_Or -> (
let t' = match_token t Tok_Or in 
let (t'',e1) = parse1 t' in
(t'',Or(e2,e1))
)
| _-> (t,e2)

and parse2 toks =
let (t,e3) = parse3 toks in
match lookahead t with
| Tok_And -> (
let t' =match_token t Tok_And in
let (t'', e2) = parse2 t' in 
(t'', And(e3,e2))
)
| _-> (t,e3)

and parse3 toks = 
let (t,e4) = parse4 toks in
match lookahead t with 
| Tok_Equal -> (
let t' = match_token t Tok_Equal in
let (t'',e3)= parse3 t' in
(t'', Equal(e4, e3))
)
| Tok_NotEqual -> (
let t' =match_token t Tok_NotEqual in
let (t'',e3) =parse3 t' in 
(t'', NotEqual(e4,e3))
)
| _-> (t,e4)

and parse4 toks = 
let (t,e5) =parse5 toks in
match lookahead t with 
| Tok_Less -> (
let t' = match_token t Tok_Less in
let (t'',e4)= parse4 t' in
(t'', Less(e5, e4))
) 
| Tok_Greater -> (
let t' = match_token t Tok_Greater in
let (t'',e4)= parse4 t' in
(t'', Greater(e5, e4))
)
| Tok_GreaterEqual -> (
let t' = match_token t Tok_GreaterEqual in
let (t'',e4)= parse4 t' in
(t'', GreaterEqual(e5, e4))
)
| Tok_LessEqual -> (
let t' = match_token t Tok_LessEqual in
let (t'',e4)= parse4 t' in
(t'', LessEqual(e5, e4))
)
| _-> (t,e5)

and parse5 toks = 
let (t,e6) =parse6 toks in 
match lookahead t with 
| Tok_Add ->  (
let t' = match_token t Tok_Add in
let (t'',e5)= parse5 t' in
(t'', Add(e6, e5))
)
| Tok_Sub ->  (
let t' = match_token t Tok_Sub in
let (t'',e5)= parse5 t' in
(t'', Sub(e6, e5))
)
| _-> (t,e6)

and parse6 toks = 
let (t,e7) =parse7 toks in 
match lookahead t with 
| Tok_Mult ->  (
let t' = match_token t Tok_Mult in
let (t'',e6)= parse6 t' in
(t'', Mult(e7, e6))
)
| Tok_Div ->  (
let t' = match_token t Tok_Div in
let (t'',e6)= parse6 t' in
(t'', Div(e7, e6))
)
| _-> (t,e7)

and parse7 toks = 
let (t,e8) = parseb8 toks in 
match lookahead t with 
| Tok_Pow -> (
let t' = match_token t Tok_Pow in
let (t'',e7)= parse7 t' in
(t'', Pow(e8, e7))
)
| _-> (t,e8)

and parseb8 toks =
match lookahead toks with
| Tok_Not -> (
let t = match_token toks Tok_Not in 
let (t',exp) =parse8 t in  
(t', Not(exp))
)
| _->parse8 toks

and parse8 toks =
match lookahead toks with
| Tok_LParen -> (
let t =match_token toks Tok_LParen in
let (t',s) =parse1 t in
let t'' = match_token t' Tok_RParen in 
(t'',s)
)
| Tok_ID s -> (
let t = match_token toks (Tok_ID s) in
(t, ID(s))
)
| Tok_Int n ->(
let t=match_token toks (Tok_Int n) in
(t, Int(n))
)
|Tok_Bool b -> (
let t=match_token toks (Tok_Bool b) in 
(t, Bool(b))
)
| _-> raise (InvalidInputException("parse_expr"))



let rec parse_stmt toks =
parsethis0 toks

and parsethis0 toks=
let (t, e0) = parsethis1 toks in
if e0 = NoOp then
(t, NoOp)
else 
let (t1,e1) =parsethis0 t in 
(t1, Seq(e0,e1)) 


and parsethis1 toks =
match lookahead toks with
| Tok_Int_Type -> (
let t =match_token toks Tok_Int_Type in
match lookahead t with
|(Tok_ID string)->
let t' =match_token t (Tok_ID string) in
if lookahead t' = Tok_Semi then
let t'' = match_token t' Tok_Semi in 
(t'', Declare(Int_Type,string))
else raise (InvalidInputException("int"))
| _-> raise (InvalidInputException("int"))
)

| Tok_Bool_Type ->  (
let t =match_token toks Tok_Bool_Type in
 match lookahead t with
|(Tok_ID string)->
let t' =match_token t (Tok_ID string) in
if lookahead t' =Tok_Semi then
let t'' =match_token t' Tok_Semi in 
(t'', Declare(Bool_Type,string))
else raise (InvalidInputException("bool"))
| _->  raise (InvalidInputException("bool"))
)
| Tok_ID(s) -> (
let t =match_token toks (Tok_ID s) in 
let t'=match_token t Tok_Assign in 
let (t'',exp) = parse_expr t' in
if lookahead t'' = Tok_Semi then
let t3 =match_token t'' Tok_Semi in
(t3, Assign(s,exp))
else raise (InvalidInputException("id"))
)
| Tok_Print -> (
let t=match_token toks Tok_Print in
let (t', exp) = parse_expr t in
if lookahead t' =Tok_Semi then 
let t'' =match_token t' Tok_Semi in
(t'', Print(exp))
else raise (InvalidInputException("print"))
)
| Tok_If -> (
let t =match_token toks Tok_If in
let (t',exp) = parse_expr t in
if not(lookahead t' = Tok_LBrace) then raise (InvalidInputException("if"))
else
let t'' = match_token t' Tok_LBrace in 
let (t3, a) = parse_stmt t'' in
if not(lookahead t3 = Tok_RBrace) then raise (InvalidInputException("if"))
else
let t4 =match_token t3 Tok_RBrace in

if lookahead t4 = Tok_Else then 
let t5 =match_token t4 Tok_Else in
if not(lookahead t5 = Tok_LBrace) then raise (InvalidInputException("if"))
else
let t6 =match_token t5 Tok_LBrace in 
let (t7, b) = parse_stmt t6 in
if not(lookahead t7 = Tok_RBrace) then raise (InvalidInputException("if"))
else
let t8 = match_token t7 Tok_RBrace in 
(t8, If(exp,a,b))

else (t4, If(exp,a,NoOp))
)
| Tok_While -> (
let t = match_token toks Tok_While in
let (t1,exp)= parse_expr t in
if not(lookahead t1 = Tok_LBrace) then raise (InvalidInputException("while"))
else
let t2 =match_token t1 Tok_LBrace in
let (t3,stt)= parse_stmt t2 in
if not(lookahead t3 = Tok_RBrace) then raise (InvalidInputException("while"))
else
let t4 = match_token t3 Tok_RBrace in
(t4, While(exp,stt))
)
| Tok_Do -> (
let t =match_token toks Tok_Do in
if not(lookahead t = Tok_LBrace) then raise (InvalidInputException("do"))
else
let t1= match_token t Tok_LBrace in 
let (t2, stt) = parse_stmt t1 in
if not(lookahead t2 = Tok_RBrace) then raise (InvalidInputException("do"))
else
let t3 = match_token t2 Tok_RBrace in
if not(lookahead t3 = Tok_While) then raise (InvalidInputException("dowhile"))
else
let t4 = match_token t3 Tok_While in
let (t5,exp)= parse_expr t4 in

if not(lookahead t5 = Tok_Semi) then raise (InvalidInputException("dowhile"))
else
let t6= match_token t5 Tok_Semi in 
(t6, DoWhile(stt, exp))
) 

| _ -> (toks, NoOp)



 

let parse_main toks =
let t = match_token toks Tok_Int_Type in
let t1 =match_token t Tok_Main in
let t2 =match_token t1 Tok_LParen in
let t3 =match_token t2 Tok_RParen in
let t4 =match_token t3 Tok_LBrace in
let (t5,stt) = parse_stmt t4 in
let t6 =match_token t5 Tok_RBrace in 
if lookahead t6 = EOF then
stt
else raise (InvalidInputException("noEOF"));;

