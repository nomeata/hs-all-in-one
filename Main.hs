{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import System.Environment
import System.IO
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.Exts
import Language.Haskell.Exts.CPP
import Language.Haskell.Names
import Data.Generics
import Data.List
import Debug.Trace
import Data.Char
import Data.Hashable
import Control.Monad
import Control.Monad.State.Strict

import EnvFromTxt

packageTxtFiles =
    [ "/nix/store/zcbmzz2drq5cii4ikqynk163v7cg8z97-ghc-9.0.2-doc/share/doc/ghc/html/libraries/base-4.15.1.0/base.txt"
    , "/nix/store/zcbmzz2drq5cii4ikqynk163v7cg8z97-ghc-9.0.2-doc/share/doc/ghc/html/libraries/containers-0.6.4.1/containers.txt"
    ]

cpphsOptions = defaultCpphsOptions
    { includes =
        [ "./.hadrian_ghci/stage0/compiler/build/"
        , "./_build/stage1/rts/build/include/"
        , "./compiler/"
        , "./rts/include/"
        ]
    , defines =
      [ ("__GLASGOW_HASKELL__","902")
      ]
    }

parseMode = defaultParseMode
    { extensions = EnableExtension <$>
        [ FlexibleContexts, BangPatterns, ExplicitForAll, ScopedTypeVariables
        , ExistentialQuantification, MultiParamTypeClasses ]
    , fixities = Just []
    }

main :: IO ()
main = do
    filenames <- getArgs
    modules <- for (zip [1..] filenames) $ \(i, fn) -> do
        hPutStrLn stderr $ "Parsing ("<> show i <> "/" <> show (length filenames) <> ")"  <> fn
        r <- parseFileWithCommentsAndCPP cpphsOptions parseMode fn
        case r of
            ParseOk (mod, _comments) -> return mod
            ParseFailed loc str -> fail $
                "Parse failed at (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

    let ours = S.fromList $ map getName modules
    unless (S.size ours == length modules) $ hPutStrLn stderr "Warning: Duplicated module names"

    env0 <- mconcat <$> mapM load packageTxtFiles
    let set_funs = [ "empty", "singleton", "member", "fromList", "isSubsetOf"
                   , "filter", "foldl'", "foldr", "elems", "size", "null"]
    let map_funs = [ "toList","fromList", "keys", "elems", "filter"
                   , "filterWithKeys", "foldlWithKey'", "foldMapWithKey", "foldr"
                   , "foldl'", "mapWithKey", "map", "isSubmapOf", "intersection"
                   , "difference", "unionWithKey", "union", "adjust", "alter"
                   , "delete", "insert", "singleton", "empty", "findWithDefault"
                   , "lookup", "member", "size", "null", "toAscList", "findMax", "findMin"
                   ]
    let env1 = M.unionsWith (<>) $ env0 :
          [ M.singleton (ModuleName () m) [Value (ModuleName () m) (Ident () v) | v <- vs]
          | (m,vs) <-
              [ ("Data.Map", map_funs)
              , ("Data.IntMap", map_funs)
              , ("Data.Map.Strict", map_funs)
              , ("Data.IntMap.Strict", map_funs)
              , ("Data.Set", set_funs)
              , ("Data.IntSet", set_funs)
              , ("Control.Monad.Trans.State.Lazy", ["StateT", "modify'"])
              , ("Control.Monad.Trans.State.Strict", ["State", "modify'"])
              , ("Control.Monad.Trans.State", ["StateT", "modify'"])
              , ("Control.Monad.Trans.Class", ["lift"])
              , ("Data.List.NonEmpty", ["groupWith", "iterate"])
              , ("Array", ["accumArray", "array"])
              , ("Data.Array.Unboxed", ["accumArray"])
              ]
          ] ++
          [ M.singleton (ModuleName () m) [Data (ModuleName () m) (Ident () v) | v <- vs]
          | (m,vs) <-
              [ ("Control.Monad.Trans.State.Lazy", ["StateT"])
              , ("Control.Monad.Trans.State.Strict", ["StateT"])
              , ("Control.Monad.Trans.State", ["StateT"])
              ]
          ]

    let env = resolve modules env1

    -- Do not annotate imports, does not work well with partial type information about base
    let imports =
            nub $
            sortOn importModule
            [ () <$ imp
            | Module _ _ _ imps _ <- modules
            , imp <- imps
            , getName' (importModule imp) `notElem` ours
            ]
    hPutStrLn stderr $ "Import decls: " <> show (length imports)

    hPutStrLn stderr "Mangling source"
    -- hPrint stderr env
    -- hPrint stderr ours
    let mangled =
            rename ours .
            -- ((()<$) <$>) .
            annotate env
            <$> modules

    -- We qualify names using data from loadBase, which may require some additional imports
    let used_modules :: S.Set String = foldMap (getQualifiedModules ours) mangled
    hPutStrLn stderr $ "External modules used: " <> show (S.size used_modules)

    let extra_imports =
            [ ImportDecl () (ModuleName () m) True False False Nothing Nothing Nothing
            | m <- S.toList used_modules ]

    let combined = combine (imports <> extra_imports) ((() <$) <$> mangled)
    hPutStrLn stderr $ "Writing combined file"
    putStrLn $ prettyPrint combined
    hPutStrLn stderr $ "Done"

getName (Module _ Nothing _ _ _) = "Main"
getName (Module _ (Just (ModuleHead _ (ModuleName _ n) _ _ )) _ _ _) = n

getName' (ModuleName _ n) = n

getQualifiedModules :: S.Set String -> Module (Scoped SrcSpanInfo) -> S.Set String
getQualifiedModules ours = foldl' go S.empty
  where
    go set (Scoped (GlobalSymbol s _) _)
        | let m = unInternalize $ getName' (symbolModule s)
        , m `S.notMember` ours
        = S.insert m set
    go set _ = set

unInternalize :: String -> String
unInternalize "Data.Typeable.Internal" = "Data.Typeable"
unInternalize "Foreign.ForeignPtr.Imp" = "Foreign.ForeignPtr"
unInternalize "GHC.Integer.Type" = "GHC.Integer"
unInternalize m = m

combine imps mods = Module () (Just (ModuleHead () (ModuleName () "Main") Nothing Nothing)) prags imps decls
  where
    prags = nub $ [ prag'
        | Module _ _ prags _ _ <- mods
        , prag <- prags
        , let prag' = filterPrag prag
        , not (isEmptyPrag prag')
        ]
    decls = concat [ decls | Module _ _ _ _ decls <- mods ]

filterPrag (LanguagePragma l ns) = LanguagePragma l [ n | n <- ns, not (isCpp n) ]
 where
  isCpp (Ident _ "CPP") = True
  isCpp _ = False
filterPrag p = p

isEmptyPrag (LanguagePragma _ []) = True
isEmptyPrag _ = False

rename :: S.Set String -> Module (Scoped SrcSpanInfo) -> Module (Scoped SrcSpanInfo)
rename ours x =
    everywhere (mkT (renameName ours (getName x)) `extT` unqualLocalQName ours) $
    everywhere (mkT fixGadtDecl) $
    everywhere (mkT fixDecl) $
    everywhere (mkT fixBinds) $
    x

unqualLocalQName :: S.Set String -> QName (Scoped SrcSpanInfo) -> QName (Scoped SrcSpanInfo)
unqualLocalQName ours (Qual l _ n)
    | Scoped (GlobalSymbol s _) _ <- l
    , let m = getName' (symbolModule s)
    , m `S.member` ours
    = UnQual l n
unqualLocalQName ours (Qual l _ n)
    | Scoped (GlobalSymbol s _) _ <- l
    , let m = unInternalize $ getName' (symbolModule s)
    , m `S.notMember` ours
    = Qual l (ModuleName l m) n
unqualLocalQName ours (UnQual l n)
    | Scoped (GlobalSymbol s _) _ <- l
    , let m = unInternalize $ getName' (symbolModule s)
    , m `S.notMember` ours
    = Qual l (ModuleName l m) n
unqualLocalQName _ qn = qn

renameName :: S.Set String -> String -> Name (Scoped SrcSpanInfo) -> Name (Scoped SrcSpanInfo)
renameName ours we n =
    let n' = renameName' ours we n
    in n' -- traceShow (n,n') n'

renameName' ours we n
    | Scoped (GlobalSymbol s _) _ <- ann n
    , let m = getName' (symbolModule s)
    , m `S.member` ours
    = mangle m (ann n) (symbolName s)
    | Scoped (GlobalSymbol s _) _ <- ann n
    = n
    | Scoped (ScopeError e) _ <- ann n
    = n -- cannot resolve, so probably imported from base, leave unmodified
    | Scoped None _ <- ann n
    = fixLocal n -- not a normal name, leave unmodified
    | otherwise
    = mangle we (ann n) n
--renameName ours we n = error (prettyPrint n ++" : " ++ show (ann n))

fixGadtDecl :: GadtDecl (Scoped SrcSpanInfo) -> GadtDecl (Scoped SrcSpanInfo)
fixGadtDecl (GadtDecl l (Ident (Scoped None ss) s) a b c d)
         =  (GadtDecl l (Ident (Scoped ValueBinder ss) s) a b c d)
fixGadtDecl g = g


fixDecl :: Decl (Scoped SrcSpanInfo) -> Decl (Scoped SrcSpanInfo)
fixDecl (SpecSig l a (UnQual l2 (Ident (Scoped None ss) n)) ts)
      = (SpecSig l a (UnQual l2 (Ident (Scoped ValueBinder ss) n)) ts)
fixDecl (InlineSig l a b (UnQual l2 (Ident (Scoped None ss) n)))
      = (InlineSig l a b (UnQual l2 (Ident (Scoped ValueBinder ss) n)))
fixDecl (AnnPragma l (Ann l2 (Ident (Scoped None ss) n) e))
      = (AnnPragma l (Ann l2 (Ident (Scoped ValueBinder ss) n) e))
fixDecl d = d

fixBinds :: Binds (Scoped SrcSpanInfo) -> Binds (Scoped SrcSpanInfo) 
fixBinds (BDecls l ds) = BDecls l (map go ds)
  where
    go (TypeSig l ns t) = (TypeSig l (goN <$> ns) t)
    go d = d
    goN (Ident (Scoped _ ss) n) = Ident (Scoped ValueBinder ss) n
    goN i = i
fixBinds b = b


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
tosymbol x | x < 0 = ":" <> tosymbol (-x)
tosymbol x = map ("⚛☃⚾♛♬☏⚒☸☀☮☘☭∞∃" !!) . map (subtract (ord '0')) . map ord . show $ x

mangle :: String -> l2 -> Name l1 -> Name l2
mangle _ l i@(Ident _ "main") = l <$ i -- leave main in place
mangle m l (Ident  _ s) =  Ident l  $ withMagicHash s (\s' -> doubleUS s' ++ "_" ++ dotToUS (doubleUS m))
mangle m l (Symbol _ s) =  Symbol l $ withMagicHash s (\s' -> doubleCol s ++ ":" ++ tosymbol (hash m))

fixLocal :: Name l -> Name l
fixLocal (Ident l "rec") = Ident l "recc" -- avoid clash with rec keyword
fixLocal n = n

withMagicHash :: String -> (String -> String) -> String
withMagicHash s f
    | "#" `isSuffixOf` s = f (init s) <> "#"
    | otherwise          = f s
