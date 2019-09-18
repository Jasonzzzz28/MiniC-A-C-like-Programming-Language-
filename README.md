# MINI C
Mini C is a C-like prgramming language. The project is an Ocaml program which includes an interpreter, lexer and parser. 

##  The Lexer (Scanner)
Before the parser processes input, the raw file must be transformed into logical units called tokens. The program implements the function `tokenize : string -> token list` which takes as input the program as a string and outputs the associated token list. The `token` type is implemented in [`tokenTypes.ml`][token types], and is defined as follows:
```
type token =
  | Tok_Do
  | Tok_While
  | Tok_Int_Type
  | Tok_Bool_Type
  | Tok_Sub
  | Tok_Semi
  | Tok_RParen
  | Tok_RBrace
  | Tok_Print
  | Tok_Pow
  | Tok_Add
  | Tok_Or
  | Tok_NotEqual
  | Tok_Not
  | Tok_Mult
  | Tok_Main
  | Tok_LessEqual
  | Tok_Less
  | Tok_LParen
  | Tok_LBrace
  | Tok_Int of int
  | Tok_If
  | Tok_ID of string
  | Tok_GreaterEqual
  | Tok_Greater
  | Tok_Equal
  | Tok_Else
  | Tok_Div
  | Tok_Bool of bool
  | Tok_Assign
  | Tok_And
  | EOF
```

## The Parser
Once the program has been transformed from a string of raw characters into more manageable tokens, the parser will parse the tokens. The parser is implemented in `parser.ml` in accordance with the signatures for `parse_expr`, `parse_stmt` and `parse_main` found in `parser.mli`.
### `parse_expr`
Expressions are a self-contained subset of the MiniC grammar. 
```
type expr =
  | ID of string
  | Int of int
  | Bool of bool
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of  expr * expr
  | Greater of expr * expr
  | Less of expr * expr
  | GreaterEqual of expr * expr
  | LessEqual of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Not of expr
```

The (ambiguous) CFG of expressions is as follows:

- Expr -> OrExpr
- OrExpr -> OrExpr `||` OrExpr | AndExpr
- AndExpr -> AndExpr `&&` AndExpr | EqualityExpr
- EqualityExpr -> EqualityExpr EqualityOperator EqualityExpr | RelationalExpr
  - EqualityOperator -> `==` | `!=`
- RelationalExpr -> RelationalExpr RelationalOperator RelationalExpr | AdditiveExpr
  - RelationalOperator -> `<` | `>` | `<=` | `>=`
- AdditiveExpr -> AdditiveExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  - AdditiveOperator -> `+` | `-`
- MultiplicativeExpr -> MultiplicativeExpr MultiplicativeOperator MultiplicativeExpr | PowerExpr
  - MultiplicativeOperator -> `*` | `/`
- PowerExpr -> PowerExpr `^` PowerExpr | UnaryExpr
- UnaryExpr -> `!` UnaryExpr | PrimaryExpr
- PrimaryExpr -> *`Tok_Int`* | *`Tok_Bool`* | *`Tok_ID`* | `(` Expr `)`
## `parse_stmt`
The next step to parsing is to build statements up around the expression parser. 
the `stmt` type:

```
type stmt =
  | NoOp
  | Seq of stmt * stmt
  | Declare of data_type * string
  | Assign of string * expr
  | If of expr * stmt * stmt
  | DoWhile of stmt * expr
  | While of expr * stmt
  | Print of expr
```

The (ambiguous) CFG of expressions is as follows:
- Stmt -> Stmt Stmt | DeclareStmt | AssignStmt | PrintStmt | IfStmt | DoWhileStmt | WhileStmt
  - DeclareStmt -> BasicType ID `;`
    - BasicType -> `int` | `bool`
  - AssignStmt -> ID `=` Expr `;`
  - PrintStmt -> `printf` `(` Expr `)` `;`
  - IfStmt -> `if` `(` Expr `)` `{` Stmt `}` ElseBranch
    - ElseBranch -> `else` `{` Stmt `}` | ε
  - DoWhileStmt -> `do` `{` Stmt `}` `while` `(` Expr `)` `;`
  - WhileStmt -> `while` `(` Expr `)` `{` Stmt `}`

## The Interpreter 
The definitional interpreter includes two functions, `eval_expr` and `eval_stmt` in the file [`eval.ml`][src/eval.ml]. **This is the only file you should modify.**

`eval_stmt : environment -> stmt -> environment`

`eval_expr : environment -> expr -> value`

### Expressions

#### Int

Integer literals evaluate to a `Int_Val` of the same value.

#### Bool

Boolean literals evaluate to a `Bool_Val` of the same value.

#### ID

An identifier evaluates to whatever value it is mapped to by the environment. Should raise a `DeclareError` if the identifier has no binding.

#### Add, Sub, Mult, Div, and Pow

*These rules are jointly classified as BinOp-Int in the formal semantics.*

These mathematical operations operate only on integers and produce a `Int_Val` containing the result of the operation. All operators must work for all possible integer values, positive or negative, except for division, which will raise a `DivByZeroError` exception on an attempt to divide by zero. If either argument to one of these operators evaluates to a non-integer, a `TypeError` should be raised.

#### Or and And

*These rules are jointly classified as BinOp-Bool in the formal semantics.*

These logical operations operate only on booleans and produce a `Bool_Val` containing the result of the operation. If either argument to one of these operators evaluates to a non-boolean, a `TypeError` should be raised.

#### Not

The unary not operator operates only on booleans and produces a `Bool_Val` containing the negated value of the contained expression. If the expression in the `Not` is not a boolean (and does not evaluate to a boolean), a `TypeError` should be raised.

#### Greater, Less, GreaterEqual, LessEqual

*These rules are jointly classified as BinOp-Int in the formal semantics*

These relational operators operate only on integers and produce a `Bool_Val` containing the result of the operation. If either argument to one of these operators evaluates to a non-integer, a `TypeError` should be raised.

#### Equal and NotEqual

These equality operators operate both on integers and booleans, but both subexpressions must be of the same type. The operators produce a `Bool_Val` containing the result of the operation. If the two arguments to these operators do not evaluate to the same type (i.e. one boolean and one integer), a `TypeError` should be raised.

### 3.3. Function 2: eval_stmt

`eval_stmt` takes an environment `env` and a statement `s` and produces an updated `environment` (defined in Types) as a result. This environment is represented as `a` in the formal semantics, but will be referred to as the environment in this document.

#### NoOp

`NoOp` is short for "no operation" and should do just that - nothing at all. It is used to terminate a chain of sequence statements, and is much like the empty list in OCaml in that way. The environment should be returned unchanged when evaluating a `NoOp`.

#### Seq

The sequencing statement is used to compose whole programs as a series of statements. When evaluating `Seq`,  evaluate the first substatement under the environment `env` to create an updated environment `env'`. Then, evaluate the second substatement under `env'`, returning the resulting environment.

#### Declare

The declaration statement is used to create new variables in the environment. If a variable of the same name has already been declared, a `DeclareError` should be raised. Otherwise, if the type being declared is `Int_Type`, a new binding to the value `Int_Val(0)` should be made in the environment. If the type being declared is `Bool_Type`, a new binding to the value `Bool_Val(false)` should be made in the environment. The updated environment should be returned.

#### Assign

The assignment statement assigns new values to already-declared variables. If the variable hasn't been declared before assignment, a `DeclareError` should be raised. If the variable has been declared to a different type than the one being assigned into it, a `TypeError` should be raised. Otherwise, the environment should be updated to reflect the new value of the given variable, and an updated environment should be returned.

#### If

The `if` statement consists of three components - a guard expression, an if-body statement and an else-body statement. The guard expression must evaluate to a boolean - if it does not, a `TypeError` should be raised. If it evaluates to true, the if-body should be evaluated. Otherwise, the else-body should be evaluated instead. The environment resulting from evaluating the correct body should be returned.

#### While

The while statement consists of two components - a guard expression and a body statement. The guard expression must evaluate to a boolean - if it does not, a `TypeError` should be raised. If it evaluates to `true`, the body should be evaluated to produce a new environment and the entire loop should then be evaluated again under this new environment, returning the environment produced by the reevaluation. If the guard evaluates to `false`, the current environment should simply be returned.

#### DoWhile

The do-while statement consists of two components - a body statement and a guard expression. The guard expression must evaluate to a boolean - if it does not, a `TypeError` should be raised. The body should be evaluated to produce a new environment, and if the guard expression evaluates to `true` in this new environment, the entire loop should then be evaluated again in the new environment, returning the environment produced by the reevaluation. If the guard evaluates to `false`, the new environment should be returned.

#### Print

First, the expression to `Print` should be evaluated. Integers should print in their natural forms (i.e. printing `Int_Val(10)` should print "10". Booleans should print in plaintext (i.e. printing `Bool_Val(true)` should print "true" and likewise for "false"). Whatever is printed should always be followed by a newline.

