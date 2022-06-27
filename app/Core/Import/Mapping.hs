module Core.Import.Mapping where
  import qualified Data.Map as M
  import Core.Parser.AST
  import Core.Parser.Parser
  import System.FilePath
  import Text.Parsec.Error
  import Data.Foldable

  type Path = String
  type ImportMap = M.Map String Statement

  insertImport :: (String, Statement) -> ImportMap -> ImportMap
  insertImport (path, statements) = M.insert path statements . M.delete path

  merge :: [(String, Statement)] -> ImportMap -> ImportMap
  merge xs ys = foldl (flip insertImport) ys xs

  resolve :: Path -> Statement -> IO (Either ParseError ImportMap)
  resolve p (Sequence s) = do
    imports <- mapM (resolve p) s
    return $ foldlM (\acc x -> fmap (\x -> merge (M.toList x) acc) x) M.empty imports
  resolve p (Import path) = do
    content <- readFile (p </> path)
    case parseLove content of
      Left err -> return $ Left err
      Right s -> do
        imports <- resolve (takeDirectory (p </> path)) s
        return $ insertImport (p </> path, s) <$> imports
  resolve _ _ = return . Right $ M.empty