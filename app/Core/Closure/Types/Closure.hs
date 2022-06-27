module Core.Closure.Types.Closure where
  import Core.Parser.AST

  data Closure = Closure {
    name :: String,
    environment :: [String],
    arguments :: [String],
    body :: Statement
  }