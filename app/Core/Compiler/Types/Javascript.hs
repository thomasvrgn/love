module Core.Compiler.Types.Javascript where
  data Literal
    = LInt Integer
    | LFloat Float
    | LString String
    | LBool Bool
    deriving (Show, Eq)

  data Javascript 
    -- Expressions
    = Lambda [String] Javascript
    | Var String
    | Lit Literal
    | Object [(String, Javascript)]
    | Ternary Javascript Javascript Javascript
    | Array [Javascript]
    | Index Javascript Javascript
    | Call Javascript [Javascript]
    | Property Javascript String
    | BinaryCall Javascript String Javascript

    -- Statements
    | Let String Javascript
    | Mod Javascript Javascript
    | Function String [String] Javascript
    | IfThen Javascript Javascript
    | IfThenElse Javascript Javascript Javascript
    | Return Javascript
    | Block [Javascript]
    | Throw Javascript
    | Raw String
    deriving (Show, Eq)