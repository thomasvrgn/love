module Core.Closure.Types.Pretty where
  import Core.Closure.Types.Closure
  import Data.List

  instance Show Closure where
    show (Closure name env args body) =
      "Closure " ++ show name ++ ":\n" ++
        "  Env  => " ++ show env ++ "\n" ++
        "  Args => " ++ intercalate ", " args ++ "\n" ++
        "  Body => " ++ show body ++ "\n"