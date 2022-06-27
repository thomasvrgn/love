module Core.Parser.AST.Definition where
  data Statement
    = Assign String Expression
    | Modify Expression Expression
    | Function String [String] Statement
    | Sequence [Statement]
    | Return Expression
    | Expression Expression
    | Import String
    deriving Eq

  data BinOp
    = Add | Mul
    | Neg | Div
    deriving Eq

  data Expression
    -- Literal value
    = Number Integer
    | String String
    | Float Float
    | Var String
    -- Function and call related
    | Bin BinOp Expression Expression
    | Call Expression [Expression]
    | Lambda [String] Statement
    -- Object related
    | Struct [(String, Expression)]
    | Property Expression String
    | List [Expression]
    | Index Expression Expression
    deriving Eq