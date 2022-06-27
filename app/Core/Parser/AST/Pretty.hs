module Core.Parser.AST.Pretty where
  import Core.Parser.AST.Definition
  import Core.Utility.Color
  import Data.List

  instance Show BinOp where
    show Add = "+"
    show Neg = "-"
    show Mul = "*"
    show Div = "/"

  instance Show Expression where
    show (Number i) = bYellow $ show i
    show (String s) = bGreen $ show s
    show (Float f)  = bYellow $ show f
    show (Var n)    = bold n

    show (Bin op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (Call n es) = show n ++ "(" ++ intercalate ", " (map show es) ++ ")"
    show (Lambda args e) = bBlue "func" ++ "(" ++ intercalate ", " args ++ ")" ++ show e 
    show (Struct fields) = bBlue "struct" ++ " { " ++ intercalate ", " (map (\(n, t) -> n ++ ": " ++ show t) fields) ++ " }"
    show (Property e n) = show e ++ "." ++ n
    show (Index e i) = show e ++ "[" ++ show i ++ "]"
    show (List es) = "[" ++ intercalate ", " (map show es) ++ "]"

  instance Show Statement where
    show (Assign n e) = bold n ++ " := " ++ show e
    show (Modify e1 e2) = show e1 ++ " = " ++ show e2
    show (Function n args e) = bBlue "func" ++ " " ++ bold n ++ "(" ++ intercalate "," args ++ ")" ++ show e
    show (Sequence sts) = "{ " ++ concatMap ((++"; ") . show) sts ++ "}"
    show (Return e) = bBlue "return " ++ show e
    show (Expression e) = show e
    show (Import p) = bBlue "import " ++ bGreen (show p)