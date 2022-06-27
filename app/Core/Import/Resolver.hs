module Core.Import.Resolver where
  import Core.Import.Mapping
  import Core.Parser.AST
  import qualified Data.Map as M
  import Data.Maybe

  replace :: ImportMap -> Statement -> Statement 
  replace m (Sequence s) =
    let m' = M.elems m
      in Sequence $ foldl (\acc x -> case x of
        Sequence s -> s ++ acc
        x -> x : acc) s m'
  replace m x = 
    let m' = M.elems m
      in Sequence $ m' ++ [x]
  
  removeImport :: Statement -> Maybe Statement
  removeImport (Import _) = Nothing
  removeImport x = Just x

  runImportRemover :: Statement -> Statement
  runImportRemover (Import _) = Sequence []
  runImportRemover (Sequence s) = Sequence $ mapMaybe removeImport s
  runImportRemover x = x