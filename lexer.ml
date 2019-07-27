open TokenTypes

let reg_space = Str.regexp "[ \n\t]+"

let reg_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let reg_int = Str.regexp "-?[0-9]+"

(* regexps for operators paired with the appropriate token, and the string's length *)
let reg_operators : (Str.regexp * (token * int)) list =
  List.map (fun (s, t, l) -> ((Str.regexp s), (t, l)))
    [
      ("(",  Tok_LParen, 1);
      (")",  Tok_RParen, 1);
      ("{",  Tok_LBrace, 1);
      ("}",  Tok_RBrace, 1);
      ("==", Tok_Equal, 2);
      ("!=", Tok_NotEqual, 2);
("=",  Tok_Assign, 1);
(">=", Tok_GreaterEqual, 2);
      ("<=", Tok_LessEqual, 2);
      (">",  Tok_Greater, 1);
      ("<",  Tok_Less, 1);
      ("\\+",Tok_Add, 1);
      ("\\*",Tok_Mult, 1);
      ("\\^",Tok_Pow, 1);

("-", Tok_Sub, 1);
(";", Tok_Semi, 1);
("||", Tok_Or, 2);
("&&", Tok_And, 2);
("/", Tok_Div, 1)
      (* TODO: Fill in missing operators *)
    ]
;;

(* iterates through list of regexps, and returns token/length pair if one matches *)
let rec op_match s pos ops =
  match ops with
    | [] -> None
    | (re, (tk,l))::ops' ->
      if (Str.string_match re s pos) then Some (tk,l)
      else op_match s pos ops'

let tokenize str =
  let rec tok pos s acc = 
    if pos >= String.length s then
      List.rev @@ EOF::acc
    else if (Str.string_match reg_space s pos) then
      tok (pos + 1) s acc

else if (Str.string_match (Str.regexp "int[ \n\t]+") s pos) then
tok (pos+3) s (Tok_Int_Type::acc)

else if (Str.string_match (Str.regexp "bool[ \n\t]+") s pos) then
tok (pos+4) s (Tok_Bool_Type::acc)

else if (Str.string_match (Str.regexp "while[ \n\t]*(") s pos) then
tok (pos+5) s (Tok_While::acc)

else if (Str.string_match (Str.regexp "do[ \n\t]*{") s pos) then
tok (pos+2) s (Tok_Do::acc)

else if (Str.string_match (Str.regexp "printf[ \n\t]*(") s pos) then
tok (pos+6) s (Tok_Print::acc)

else if (Str.string_match (Str.regexp "![ \n\t]*(?[ \n\t]*[a-zA-Z0-9]") s pos) then
tok (pos+1) s (Tok_Not::acc)

else if (Str.string_match (Str.regexp "main[ \n\t]*(") s pos) then
tok (pos+4) s (Tok_Main::acc)

else if (Str.string_match (Str.regexp "if[ \n\t]*(") s pos) then
tok (pos+2) s (Tok_If::acc)

else if (Str.string_match (Str.regexp "else[ \n\t]*{") s pos) then
tok (pos+4) s (Tok_Else::acc)

else if (Str.string_match reg_ID s pos) then
let id_token = Str.matched_string s in
      (* TODO: Need to match keywords, booleans, etc. not just identifiers *)
      tok (pos + (String.length id_token)) s ((Tok_ID id_token)::acc)
(* TODO: Match numbers *)


else if (Str.string_match reg_int s pos) then
let int_token = Str.matched_string s in
tok (pos +(String.length int_token)) s ((Tok_Int (int_of_string int_token))::acc)

else if (Str.string_match (Str.regexp "(true)|(false)") s pos) then
(if Str.matched_string s = "true" then
tok (pos+4) s ((Tok_Bool true)::acc)
else
tok (pos+5) s ((Tok_Bool false)::acc) )

else
   match (op_match s pos reg_operators) with
   Some (tk, len) -> tok (pos + len) s (tk::acc)
       | None -> raise (InvalidInputException "tokenize") 
in

    tok 0 str []
