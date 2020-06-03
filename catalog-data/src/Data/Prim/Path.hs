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
       , checkAndRemExt
       , remCommonPathPrefix
       , isPathPrefix
       , nullPath
       , showPath
       , viewTop
       , viewBase
       , quotePath
       , msgPath
       , listToPath
       , listFromPath
       , isoPathList
       , checkExtPath
       )
where

import Data.Digest.Murmur64 (Hashable64(..))
import Data.Prim.Name
import Data.Prim.Prelude
import qualified Data.Text as T

-- ----------------------------------------

data Path' n = BN !n
             | DN !n !(Path' n)

type Path = Path' Name

readPath :: String -> Path
readPath ('/' : xs0)
  = buildPath . tokenize $ xs0
  where
    tokenize :: String -> [String]
    tokenize [] = []
    tokenize xs = xs1 : tokenize (drop 1 xs2)
      where
        (xs1, xs2) = span (not . (== '/')) xs

    buildPath :: [String] -> Path
    buildPath ("." : xs) = buildPath xs
    buildPath [xs]       = BN $ mkName xs
    buildPath (x : xs)   = DN (mkName x) (buildPath xs)
    buildPath []         = emptyPath

readPath ""
  = emptyPath

readPath xs
  = readPath ('/' : xs)

mkPath :: n -> Path' n
mkPath = BN
{-# INLINE mkPath #-}

emptyPath :: Monoid n => Path' n
emptyPath = mkPath mempty
{-# INLINE emptyPath #-}

listToPath :: [Text] -> Path
listToPath = foldr (consPath . (isoText #)) emptyPath

listFromPath :: Path -> [Text]
listFromPath (BN n)
  | n == mempty       = []
  | otherwise         = [n ^. isoText]
listFromPath (DN n p) = n ^. isoText : listFromPath p

isoPathList :: Iso' Path [Text]
isoPathList = iso listFromPath listToPath

nullPath :: (Monoid n, Eq n) => Path' n -> Bool
nullPath (BN n)
  | n == mempty = True
nullPath _      = False

infixr 5 `consPath`
infixr 5 `snocPath`
infixr 5 `concPath`

consPath :: (Monoid n, Eq n) => n -> Path' n -> Path' n
consPath n p
  | n == mempty = p
  | isempty p   = mkPath n
  | otherwise   = DN n p

snocPath :: (Monoid n, Eq n) => Path' n -> n -> Path' n
snocPath p n = p `concPath` mkPath n

concPath :: (Monoid n, Eq n) => Path' n -> Path' n -> Path' n
concPath (BN n) p2    = consPath n p2
concPath (DN n p1) p2 = consPath n $ concPath p1 p2

viewBase :: (Monoid n, Eq n) => Iso' (Path' n) (Path' n, n)
viewBase = iso toPair (uncurry snocPath)
  where
    toPair (BN n)   = (emptyPath, n)
    toPair (DN n p) = (n `consPath` p', n')
      where
        (p', n') = toPair p
{-# INLINE viewBase #-}

viewTop :: (Monoid n, Eq n) => Iso' (Path' n) (n, Path' n)
viewTop = iso toPair (uncurry consPath)
  where
    toPair (DN n p) = (n, p)
    toPair (BN n)   = (n, emptyPath)
{-# INLINE viewTop #-}

headPath :: (Monoid n, Eq n) => Path' n -> n
headPath = (^. viewTop . _1)
{-# INLINE headPath #-}

tailPath :: (Monoid n, Eq n) => Path' n -> Path' n
tailPath = (^. viewTop . _2)
{-# INLINE tailPath #-}

substPathName :: (Monoid n, Eq n) => n -> Path' n -> Path' n
substPathName n p = p & viewBase . _2 .~ n
{-# INLINE substPathName #-}

isPathPrefix :: (Eq n, Monoid n) => Path' n -> Path' n -> Bool
isPathPrefix p1 p2
  | n1 /= n2         = False
  | nullPath p1'     = True
  | nullPath p2'     = False
  | otherwise        = isPathPrefix p1' p2'
  where
    (n1, p1') = p1 ^. viewTop
    (n2, p2') = p2 ^. viewTop

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

checkAndRemExt :: (Monoid n, Eq n, IsoString n)
               => String -> Path' n -> Maybe (Path' n)
checkAndRemExt ext p
  | rext `isPrefixOf` rbn = Just $ viewBase # (dp, isoString # bn')
  | otherwise             = Nothing
  where
    (dp, bn) = p ^. viewBase
    rbn  = bn ^. isoString . to reverse
    rext = reverse ext
    bn'  = reverse . drop (length ext) $ rbn


remCommonPathPrefix :: (Monoid n, Eq n) => Path' n -> Path' n -> (Path' n, Path' n)
remCommonPathPrefix p1 p2
  | n1 /= n2 = (p1, p2)
  | nullPath p1' = (p1', p2')
  | nullPath p2' = (p1', p2')
  | otherwise    = remCommonPathPrefix p1' p2'
  where
    (n1, p1') = p1 ^. viewTop
    (n2, p2') = p2 ^. viewTop

showPath :: (Monoid n, Eq n, Show n) => Path' n -> String
showPath (BN n)
  | n == mempty   = ""
  | otherwise     = "/" ++ show n

showPath (DN n p) = "/" ++ show n ++ showPath p

quotePath :: (Monoid n, Eq n, Show n) => Path' n -> String
quotePath = show . showPath


msgPath :: Path -> Text -> Text
msgPath p msg =
  T.unwords [msg, "\"" <> toText' p <> "\""]
  where
    toText' (BN n)
      | isempty n     = mempty
      | otherwise     = "/" <> n ^. isoText
    toText' (DN n p') = "/" <> n ^. isoText <> toText' p'

checkExtPath :: Text -> Path -> Bool
checkExtPath ext p
  = ext == ext'
    &&
    T.length bn > ln
  where
    ln   = T.length ext
    bn   = (p ^. viewBase . _2) ^. isoText
    ext' = T.toLower . T.takeEnd ln $ bn

deriving instance Eq  n => Eq  (Path' n)
deriving instance Ord n => Ord (Path' n)

instance (Monoid n, Eq n) => Semigroup (Path' n) where
  (<>) = concPath

instance (Monoid n, Eq n) => Monoid (Path' n) where
  mempty = emptyPath
  mappend = (<>)
  {-# INLINE mappend #-}
  {-# INLINE mempty #-}

instance (Monoid n, Eq n) => IsEmpty (Path' n) where
  isempty = (== emptyPath)
  {-# INLINE isempty #-}

instance IsoString Path where
  isoString = iso showPath readPath
  {-# INLINE isoString #-}

instance IsoText Path

instance (Eq n, Monoid n, Show n) => Show (Path' n) where
  show = showPath
  {-# INLINE show #-}

instance (Eq n, Monoid n, Show n) => ToJSON (Path' n) where
  toJSON = toJSON . showPath
  {-# INLINE toJSON #-}

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath
  {-# INLINE fromString #-}

instance (Eq n, Monoid n, Show n) => Hashable64 (Path' n) where
  hash64Add = hash64Add . showPath

-- ----------------------------------------