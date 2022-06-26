module IR where
  import Parser (Statement(..), Expression(..))
  import Data.List (intercalate)
  
  fromArgs :: [String] -> String
  fromArgs xs = "(" ++ intercalate ", " xs ++ ")"

  fromStmt :: Statement -> String
  fromStmt (Assign x e) = "let " ++ x ++ " = " ++ fromExpr e
  fromStmt (Function name args (Expression e))
    = name ++ " = " ++ fromArgs args ++ " => (" ++ fromExpr e ++ ")" 
  fromStmt (Modify x e) = fromExpr x ++ " = " ++ fromExpr e
  fromStmt (Function name args body) = name ++ " = " ++ fromArgs args ++ " => " ++ fromStmt body
  fromStmt (Sequence items) = "{" ++ intercalate "; " (map fromStmt items) ++ "}"
  fromStmt (Return v) = "return " ++ fromExpr v
  fromStmt (Expression e) = fromExpr e

  fromField :: (String, Expression) -> String
  fromField (name, expr) = name ++ ": " ++ fromExpr expr

  fromExpr :: Expression -> String
  fromExpr (Number i) = show i
  fromExpr (String s) = show s
  fromExpr (Float  f) = show f
  fromExpr (Bin op e1 e2) = "(" ++ fromExpr e1 ++ " " ++ show op ++ " " ++ fromExpr e2 ++ ")"
  fromExpr (Var s) = s
  fromExpr (Call s args) = fromExpr s ++ "(" ++ intercalate ", " (map fromExpr args) ++ ")"
  fromExpr (Struct fields) = "{" ++ intercalate ", " (map fromField fields) ++ "}"
  fromExpr (Property e s) = fromExpr e ++ "." ++ s
  fromExpr (Index e i) = fromExpr e ++ "[" ++ fromExpr i ++ "]"
  fromExpr (List xs) = "[" ++ intercalate ", " (map fromExpr xs) ++ "]"
  fromExpr (Lambda args body) = fromArgs args ++ " => " ++ fromStmt body