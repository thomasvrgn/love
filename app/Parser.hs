module Parser where
  import Text.Parsec
  import Text.Parsec.Expr
  import Text.Parsec.Char
  import Text.Parsec.String
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Language (emptyDef)
  import Debug.Trace (traceShow)
  import Text.Parsec.Token (GenTokenParser)
  import Data.Functor.Identity (Identity)

  {- AST DEFINITION -}

  data Expression
    = Number Integer
    | String String
    | Float Float
    | Bin BinOp Expression Expression
    | Var String
    | Call Expression [Expression]
    | Lambda [String] Statement
    | Struct [(String, Expression)]
    | Property String Expression
    deriving Show

  data Statement
    = Assign String Expression
    | Function String [String] Statement
    | Sequence [Statement]
    | Return Expression
    | Expression Expression
    deriving Show

  data BinOp
    = Add | Mul
    | Neg | Div
    deriving Show

  {- LEXER PART -}
  languageDef =
    emptyDef { Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter
              , Token.identLetter     = alphaNum
              , Token.reservedNames   = ["func", "return", ":=", "struct"]
              , Token.reservedOpNames = ["+", "-", "*", "/", ":", "."] }

  lexer :: GenTokenParser String u Identity
  lexer = Token.makeTokenParser languageDef

  identifier :: Parser String
  identifier = Token.identifier lexer

  reserved :: String -> Parser ()
  reserved   = Token.reserved lexer

  reservedOp :: String -> Parser ()
  reservedOp = Token.reservedOp lexer

  parens :: Parser a -> Parser a
  parens     = Token.parens lexer

  integer :: Parser Integer
  integer    = Token.integer lexer

  whiteSpace :: Parser ()
  whiteSpace = Token.whiteSpace lexer

  comma :: Parser String
  comma = Token.comma lexer

  semi :: Parser String
  semi = Token.semi lexer

  {- PARSER PART -}

  parser :: Parser Statement
  parser = whiteSpace >> statement

  -- Statement parsing

  statement :: Parser Statement
  statement
    = choice [
      assign, Expression <$> expression,
      returnE, block, function
    ]

  returnE :: Parser Statement
  returnE = do
    reserved "return"
    Return <$> expression

  block :: Parser Statement
  block = Sequence <$> Token.braces lexer (many (statement <* semi))

  assign :: Parser Statement
  assign = try $ do
    var <- identifier
    reserved ":="
    Assign var <$> expression

  function :: Parser Statement
  function = do
    reserved "func"
    name <- identifier
    args <- parens (Token.commaSep lexer identifier)
    Function name args <$> statement

  -- Expression parsing

  expression :: Parser Expression
  expression = buildExpressionParser table term

  stringLit :: Parser Expression
  stringLit = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return $ String str

  floatLit :: Parser Expression
  floatLit = do
    num <- many1 digit
    char '.'
    dec <- many1 digit
    return $ Float (read (num ++ "." ++ dec) :: Float)

  term :: Parser Expression
  term
    = try floatLit <|> (Number <$> integer) <|> stringLit <|> try lambda
   <|> call <|> try property <|> object <|> variable <|> parens expression

  property :: Parser Expression
  property 
    = Property <$> identifier <*> (char '.' *> expression)

  variable :: Parser Expression
  variable = Var <$> identifier

  lambda :: Parser Expression
  lambda = do
    reserved "func"
    args <- parens (Token.commaSep lexer identifier)
    Lambda args <$> statement

  object :: Parser Expression
  object = do
    reserved "struct"
    fields <- Token.braces lexer $ sepBy (try $ do
      name <- identifier
      reservedOp ":"
      value <- expression
      return (name, value)
      ) comma
    return $ Struct fields

  call :: Parser Expression
  call = try $ do
    e <- variable <|> parens expression
    arg <- parens $ sepBy expression (char ',')
    return $ Call e arg

  table :: [[Operator String () Identity Expression]]
  table = [
      [Infix (reservedOp "*" >> return (Bin Mul)) AssocLeft,
      Infix (reservedOp "/" >> return (Bin Div)) AssocLeft],
      [Infix (reservedOp "+" >> return (Bin Add)) AssocLeft,
      Infix (reservedOp "-" >> return (Bin Neg)) AssocLeft]
    ]
  