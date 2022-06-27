module Core.Compiler.Types.IR where
  import Core.Compiler.Types.Javascript
  import Data.List

  fromLiteral :: Literal -> String
  fromLiteral (LInt i) = show i
  fromLiteral (LFloat f) = show f
  fromLiteral (LString s) = show s
  fromLiteral (LBool b) = show b

  from :: Javascript -> String
  -- Expression compilation
  from (Lambda args e) = "function(" ++ intercalate "," args ++ "){" ++ from e ++ "}"
  from (Var e) = e
  from (Lit e) = fromLiteral e
  from (Object fields) = "{" ++ intercalate ", " (map (\(k,v) -> k ++ ":" ++ from v) fields) ++ "}"
  from (Array elements) = "[" ++ intercalate ", " (map from elements) ++ "]"
  from (Call e1 e2) = from e1 ++ "(" ++ intercalate ", " (map from e2) ++ ")"
  from (Property e1 e2) = from e1 ++ "." ++ e2
  from (BinaryCall e1 op e2) = from e1 ++ " " ++ op ++ " " ++ from e2
  from (Ternary e1 e2 e3) = from e1 ++ " ? " ++ from e2 ++ " : " ++ from e3
  from (Index e1 e2) = from e1 ++ "[" ++ from e2 ++ "]"

  -- Statement compilation
  from (Let n v) = "var " ++ n ++ " = " ++ from v
  from (Mod n v) = from n ++ " = " ++ from v
  from (Function n args e) = "var " ++ n ++ " = (" ++ intercalate "," args ++ ") =>" ++ from e
  from (IfThen cond then') = "if(" ++ from cond ++ ")" ++ from then'
  from (IfThenElse cond then' else') = "if(" ++ from cond ++ ")" ++ from then' ++ "else" ++ from else'
  from (Return v) = "return " ++ from v
  from (Block stmts) = "{" ++ intercalate ";" (map from stmts) ++ "}"
  from (Throw v) = "throw " ++ from v
  from (Raw s) = s