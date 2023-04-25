module EnvFromTxt where

import qualified Data.Map as M
import Data.Char
import Language.Haskell.Exts.Syntax (ModuleName(..), Name(..))
import Language.Haskell.Names
import Data.List (stripPrefix, isPrefixOf)

load :: FilePath -> IO Environment
load fp = do
    ls <- lines <$> readFile fp
    pure $ skip M.empty ls
  where
    skip :: Environment -> [String] -> Environment
    skip env [] = env
    skip env (l:ls)
        | Just mod <- stripPrefix "module " l
        = go env (ModuleName () mod) ls
        | otherwise
        = skip env ls

    go :: Environment -> ModuleName () -> [String] -> Environment
    go env mod [] = env
    go env mod (l:ls)
        |  all isSpace l
        || "--" `isPrefixOf` dropWhile isSpace l
        || "instance" `isPrefixOf` l
        || "infix" `isPrefixOf` l
        || "}" == l
        = go env mod ls

        | Just mod' <- stripPrefix "module " l
        = go env (ModuleName () mod') ls

        | "class" : n : _ <- words l
        = add Class n
        | "data" : n : _ <- words l
        = add Data n
        | "type" : n : _ <- words l
        = add Type n
        | "newtype" : n : _ <- words l
        = add NewType n
        | n : "::" : _ <- words l
        = add Value n
        | "pattern" : n : "::" : _ <- words l
        = add (\m n -> PatternConstructor m n Nothing) n

        | otherwise
        = error l
      where
       add k n = go (M.insertWith (<>) mod [k mod (name n)] env) mod ls

       name n
         | take 1 n == "(" && take 1 (reverse n) == ")"
         = Symbol () (init (tail n))
         | otherwise
         = Ident () n
