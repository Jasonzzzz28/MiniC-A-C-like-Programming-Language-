exception TypeError of string
exception DeclareError of string
exception DivByZeroError

val eval_expr : MiniCTypes.environment -> MiniCTypes.expr -> MiniCTypes.value
val eval_stmt : MiniCTypes.environment -> MiniCTypes.stmt -> MiniCTypes.environment
