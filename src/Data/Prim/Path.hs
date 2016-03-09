{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Prim.Path
       ( Path'
       , Path
       , readPath
       , mkPath
       , emptyPath
       , consPath
       , concPath
       , snocPath
       , tailPath
       , headPath
       , substPathName
       , substPathPrefix
       , showPath
       , path2string
       , viewTop
       , viewBase
       )
where

import Data.Digest.Murmur64 (Hashable64(..))
import Data.Prim.Name
import Data.Prim.Prelude
import Text.Regex.XMLSchema.Generic (tokenize)

-- ----------------------------------------

data Path' n = BN !n
             | DN !n !(Path' n)

type Path = Path' Name

readPath :: String -> Path
readPath ('/' : xs0)
  = buildPath . tokenize "[^/]*" $ xs0
  where
    buildPath :: [String] -> Path
    buildPath [xs] = BN $ mkName xs
    buildPath (x : xs) = DN (mkName x) (buildPath xs)
    buildPath [] = error "buildPath: empty path"

readPath ""
  = emptyPath

readPath xs
  = error $ "readPath: no path: " ++ show xs

mkPath :: n -> Path' n
mkPath = BN

emptyPath :: Monoid n => Path' n
emptyPath = mkPath mempty

infixr 5 `consPath`
infixr 5 `snocPath`
infixr 5 `concPath`

consPath :: (Monoid n, Eq n) =>
            n -> Path' n -> Path' n
consPath n p
  | n == mempty = p
  | isempty p  = mkPath n
  | otherwise   = DN n p

snocPath :: (Monoid n, Eq n) => Path' n -> n -> Path' n
snocPath p n = p `concPath` mkPath n

concPath :: (Monoid n, Eq n) =>
            Path' n -> Path' n -> Path' n
concPath (BN n) p2    = consPath n p2
concPath (DN n p1) p2 = consPath n $ concPath p1 p2

viewBase :: (Monoid n, Eq n) => Iso' (Path' n) (Path' n, n)
viewBase = iso toPair (uncurry snocPath)
  where
    toPair (BN n)   = (emptyPath, n)
    toPair (DN n p) = (n `consPath` p', n')
      where
        (p', n') = toPair p

viewTop :: (Monoid n, Eq n) => Iso' (Path' n) (n, Path' n)
viewTop = iso toPair (uncurry consPath)
  where
    toPair (DN n p) = (n, p)
    toPair (BN n)   = (n, emptyPath)

headPath :: (Monoid n, Eq n) => Path' n -> n
headPath = (^. viewTop . _1)

tailPath :: (Monoid n, Eq n) => Path' n -> Path' n
tailPath = (^. viewTop . _2)

substPathName :: (Monoid n, Eq n) => n -> Path' n -> Path' n
substPathName n p = p & viewBase . _2 .~ n

substPathPrefix :: (Monoid n, Eq n, Show n) =>
                   Path' n -> Path' n -> (Path' n -> Path' n)
substPathPrefix old'px new'px p0 =
  go old'px p0
  where
    go px p
      | isempty px = new'px `concPath` p
      | hpx == hp  = go tpx tp
      | otherwise   = error $
                      unwords [show old'px, "isn't a prefix of path", show p0]
      where
        (hpx, tpx) = px ^. viewTop
        (hp,  tp ) = p  ^. viewTop

showPath :: (Monoid n, Eq n, Show n) => Path' n -> String
showPath (BN n)
  | n == mempty   = ""
  | otherwise     = "/" ++ show n

showPath (DN n p) = "/" ++ show n ++ showPath p

path2string :: Iso' Path String
path2string = iso showPath readPath

deriving instance Eq n  => Eq  (Path' n)
deriving instance Ord n => Ord (Path' n)

instance (Monoid n, Eq n) => Monoid (Path' n) where
  mempty = emptyPath
  mappend = concPath

instance (Monoid n, Eq n) => IsEmpty (Path' n) where
  isempty = (== emptyPath)

-- deriving instance Show n => Show (Path' n)

instance (Eq n, Monoid n, Show n) => Show (Path' n) where
  show = showPath

instance (Eq n, Monoid n, Show n) => ToJSON (Path' n) where
  toJSON = toJSON . showPath

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath

instance (Eq n, Monoid n, Show n) => Hashable64 (Path' n) where
  hash64Add = hash64Add . showPath

-- ----------------------------------------
