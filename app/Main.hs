module Main where
  import Text.Parsec (runParser)
  import Parser (parser)

  main :: IO ()
  main = do
    input <- readFile "tests/example"
    let p = runParser parser () "" input
    print p