module Main where

import System.Environment
import Data.Traversable
import Language.Haskell.Exts
import Language.Haskell.Names
import Data.Generics
import Data.List
import Debug.Trace
import Data.Char
import Data.Hashable

main :: IO ()
main = do
    filenames <- getArgs
    modules <- for filenames $ \fn -> fromParseResult <$> parseFile fn
    let ours = map getName modules
    base <- loadBase
    let env = resolve modules base
    let mangled =
            (()<$) .
            rename ours .
            removeImports ours .
            ((()<$) <$>) .
            annotate env
            <$> modules
    let combined = combine mangled
    putStrLn $ prettyPrint combined

getName (Module _ Nothing _ _ _) = "Main"
getName (Module _ (Just (ModuleHead _ (ModuleName _ n) _ _ )) _ _ _) = n

getName' (ModuleName _ n) = n

removeImports :: [String] -> Module l2 -> Module l2
removeImports ours (Module l mh prags imports decls)
    = Module l mh prags (filter go imports) decls
  where go i = getName' (importModule i) `notElem` ours

combine mods = Module () (Just (ModuleHead () (ModuleName () "Main") Nothing Nothing)) prags imps decls
  where
    prags = nub $ concat [ prags | Module _ _ prags _ _ <- mods ]
    imps  = nub $ concat [ imps | Module _ _ _ imps _ <- mods ]
    decls = concat [ decls | Module _ _ _ _ decls <- mods ]

rename :: [String] -> Module (Scoped ()) -> Module (Scoped ())
rename ours x = everywhere (mkT (renameName ours (getName x)) `extT` renameQName ours) x

renameQName :: [String] -> QName (Scoped ()) -> QName (Scoped ())
renameQName ours (Qual l _ n)
    | Scoped (GlobalSymbol s _) _ <- l
    , let m = getName' (symbolModule s)
    , m `elem` ours
    = UnQual l n
renameQName _ qn = qn


renameName :: [String] -> String -> Name (Scoped ()) -> Name (Scoped ())
renameName ours we n
    | Scoped (GlobalSymbol s _) _ <- ann n
    , let m = getName' (symbolModule s)
    , m `elem` ours
    = mangle m (Scoped None () <$ symbolName s)
    | Scoped (GlobalSymbol s _) _ <- ann n
    = n
    | Scoped (ScopeError e) _ <- ann n
    = error (show e)
    | otherwise
    = mangle we n
--renameName ours we n = error (prettyPrint n ++" : " ++ show (ann n))

modOf :: String -> Scoped () -> Maybe String
modOf we (Scoped (GlobalSymbol s _) _) = Just $ getName' (symbolModule s)
modOf we (Scoped ValueBinder _)        = Just we
modOf we (Scoped (LocalValue _) _)     = Just we
modOf we _                             = Nothing

doubleUS = concatMap go
  where go '_' = "__"
        go c = [c]

doubleCol = concatMap go
  where go ':' = "::"
        go c = [c]

dotToUS = concatMap go
  where go '.' = "_"
        go c = [c]

tosymbol :: Int -> String
tosymbol = map ("⚛☃⚾♛♬☏⚒☸☀☮☘☭∞∃" !!) . map (subtract (ord '0')) . map ord . show

mangle :: String -> Name l2 -> Name l2
mangle _ i@(Ident  l "main") = i -- leave main in place
mangle m (Ident  l s) =  Ident l  $ doubleUS s ++ "_" ++ dotToUS (doubleUS m)
mangle m (Symbol l s) =  Symbol l $ doubleCol s ++ ":" ++ tosymbol (hash m)
