module Main where
  import Text.Parsec (runParser)
  import Core.Parser.Parser (parser)
  import Core.Import.Mapping
  import Core.Import.Resolver
  import System.FilePath
  import Core.Compiler.Translation
  import Core.Compiler.Types
  
  main :: IO ()
  main = do
    let path = "tests/example.love"
    input <- readFile path
    let p = runParser parser () "" input
    case p of
      Left err -> print err
      Right x -> do
        res <- resolve (takeDirectory path) x
        let x' = fmap (runImportRemover . (`replace` x)) res
        --let x'' = fmap ((`runState` (([], []), 0)) . convertStmt) x'
        --print x''
        let x'' = fmap (from . compileStmt) x'
        case x'' of
          Left err -> print err
          Right js -> writeFile (path -<.> "js") js
