module Core.Closure.Conversion where
  import Core.Closure.Types
  import Control.Monad.State
  import Data.Bifunctor
  import Core.Parser.AST

  type Environment = [String]
  type ClosureST = State ((Environment, [Closure]), Int)

  fresh :: ClosureST String
  fresh = do
    i <- gets snd
    modify . second $ (+1)
    return $ "lambda" ++ show i

  addClosure :: Closure -> ClosureST ()
  addClosure c = modify . first . second $ (c:)

  addEnv :: String -> ClosureST ()
  addEnv s = modify . first . first $ (s:)

  unionEnv :: Environment -> ClosureST ()
  unionEnv es = modify . first . first $ (es++)

  setEnv :: Environment -> ClosureST ()
  setEnv e = modify . first . first $ const e

  makeClosure :: ([String], String) -> Environment -> Expression
  makeClosure (args, name) env
    = Call (Var "make-closure") [Var name, List (map Var args), List (map Var env)]

  convertStmt :: Statement -> ClosureST Statement
  convertStmt (Function name args body) = do
    name' <- fresh
    env <- gets (fst . fst)
    addEnv name
    unionEnv args
    body' <- convertStmt body
    setEnv env
    addClosure $ Closure name' env args body'
    return . Assign name $ makeClosure (args, name') env
  
  -- Assignment section
  convertStmt (Assign name (Lambda args body)) = do
    name' <- fresh
    env <- gets (fst . fst)
    addEnv name
    unionEnv args
    body' <- convertStmt body
    setEnv env
    addClosure $ Closure name' env args body'
    return $ Assign name (makeClosure (args, name') env)
  convertStmt (Assign name value) = do
    addEnv name
    body' <- convertExpr value
    return $ Assign name body'

  -- Modify section
  convertStmt (Modify name (Lambda args body)) = do
    name' <- fresh
    env <- gets (fst . fst)
    addEnv $ getModifyName name
    unionEnv args
    body' <- convertStmt body
    setEnv env
    addClosure $ Closure name' env args body'
    return $ Modify name (makeClosure (args, name') env)
  convertStmt (Modify name value) = do
    addEnv $ getModifyName name
    body' <- convertExpr value
    return $ Modify name body'
  
  -- Other statements
  convertStmt (Return e) = Return <$> convertExpr e
  convertStmt (Sequence s) = do
    e <- gets (fst . fst)
    m <- Sequence <$> mapM convertStmt s
    setEnv e
    return m
  convertStmt (Expression e) = Expression <$> convertExpr e
  convertStmt x = return x

  convertExpr :: Expression -> ClosureST Expression
  convertExpr (Lambda args body) = do
    name' <- fresh
    env <- gets (fst . fst)
    unionEnv args
    body' <- convertStmt body
    setEnv env
    addClosure $ Closure name' env args body'
    return $ makeClosure (args, name') env
  convertExpr (Call t args) = do
    t' <- convertExpr t
    args' <- mapM convertExpr args
    return $ Call t' args'
  convertExpr (Struct fields) = do
    fields' <- mapM (convertExpr . snd) fields
    return $ Struct (zip (map fst fields) fields')
  convertExpr (Property e s) = Property <$> convertExpr e <*> pure s
  convertExpr (List xs) = List <$> mapM convertExpr xs
  convertExpr (Index e i) = Index <$> convertExpr e <*> convertExpr i
  convertExpr x = return x

  getModifyName :: Expression -> String
  getModifyName (Var name) = name
  getModifyName (Property name _) = getModifyName name
  getModifyName (Index name _) = getModifyName name
  getModifyName _ = ""