module Main where
  import Text.Parsec (runParser)
  import Parser (parser)
  import IR (fromStmt)
  
  main :: IO ()
  main = do
    input <- readFile "tests/example"
    let p = runParser parser () "" input
    case p of
      Left err -> print err
      Right x -> do
        print x
        let js = fromStmt x
        writeFile "tests/example.js" js