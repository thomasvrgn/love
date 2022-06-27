module Core.Compiler.Translation where
  import Core.Compiler.Types
  import qualified Core.Parser.AST as A
  import Data.Bifunctor

  compileStmt :: A.Statement -> Javascript
  compileStmt (A.Assign name expr) = Let name (compileExpr expr)
  compileStmt (A.Modify name expr) = Mod (compileExpr name) (compileExpr expr)
  compileStmt (A.Function n args body) = Function n args (compileStmt body)
  compileStmt (A.Sequence stmts) = Block $ map compileStmt stmts
  compileStmt (A.Expression e) = compileExpr e
  compileStmt (A.Return v) = Return $ compileExpr v
  compileStmt (A.Import s) = error "Cannot happen"

  compileExpr :: A.Expression -> Javascript
  compileExpr (A.Number i) = Lit $ LInt i
  compileExpr (A.String s) = Lit $ LString s
  compileExpr (A.Float f) = Lit $ LFloat f
  compileExpr (A.Var n) = Var n
  compileExpr (A.Bin op e1 e2) = BinaryCall (compileExpr e1) (show op) (compileExpr e2)
  compileExpr (A.Call n args) = Call (compileExpr n) (map compileExpr args)
  compileExpr (A.Lambda args body) = Lambda args (compileStmt body)
  compileExpr (A.Struct fields) = Object $ map (second compileExpr) fields
  compileExpr (A.List fields) = Array $ map compileExpr fields
  compileExpr (A.Property e n) = Property (compileExpr e) n
  compileExpr (A.Index e i) = Index (compileExpr e) (compileExpr i)