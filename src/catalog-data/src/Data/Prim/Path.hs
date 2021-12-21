{-# LANGUAGE DeriveFunctor #-}
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
  , initPath
  , lastPath
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
  , editName
  )
where

-- catalog-data
import Data.Prim.Prelude
import Data.Prim.Name
import Text.SimpleParser

-- other libs
import Data.Digest.Murmur64 (Hashable64(..))

import qualified Data.Text as T

-- ----------------------------------------
--
-- Path is a reversed strict list
-- the last part is stored in the root
--
-- Path' is used for Functor and Foldable instances

data Path' n = PNil
             | PSnoc !(Path' n) !n

type Path = Path' Name

----------------------------------------

readPath :: String -> Path
readPath = fromMaybe emptyPath <$> parseMaybe ppath
  where
    ppath :: SP Path
    ppath = listToPath <$>
      (many (single '/') >> many pname)

    pname :: SP Text
    pname = (^. isoText) <$> pstring

    pstring :: SP String
    pstring = some (noneOf' "/") <* many (single '/')


-- empty Name -> empty Path

mkPath :: Name -> Path
mkPath n
  | isempty n = PNil
  | otherwise = PSnoc PNil n
{-# INLINE mkPath #-}

emptyPath :: Path' n
emptyPath = PNil
{-# INLINE emptyPath #-}

listToPath :: [Text] -> Path
listToPath = foldl' (\ p' t' -> p' `concPath` mkPath (isoText # t')) emptyPath
{-# INLINE listToPath #-}

listFromPath :: Path -> [Text]
listFromPath = lfp []
  where
    lfp acc PNil = acc
    lfp acc (PSnoc p n) = lfp ((n ^. isoText) : acc) p
{-# INLINE listFromPath #-}

textFromPath :: Path -> Text
textFromPath = foldMap (\ n -> "/" <> n ^. isoText)
{-# INLINE textFromPath #-}

textToPath :: Text -> Path
textToPath = listToPath . T.split (== '/')

isoPathList :: Iso' Path [Text]
isoPathList = iso listFromPath listToPath
{-# INLINE isoPathList #-}

nullPath :: Path' n -> Bool
nullPath PNil = True
nullPath _    = False
{-# INLINE nullPath #-}

infixr 5 `consPath`
infixr 5 `snocPath`
infixr 5 `concPath`

consPath :: Name -> Path -> Path
consPath n p = mkPath n `concPath` p
{-# INLINE consPath #-}

snocPath :: Path -> Name -> Path
snocPath p n = p `concPath` mkPath n
{-# INLINE snocPath #-}

concPath :: Path -> Path -> Path
concPath PNil p2 = p2
concPath p1 PNil = p1
concPath p1 (PSnoc p2 n) = concPath p1 p2 `PSnoc` n

viewBase :: Iso' Path (Path, Name)
viewBase = iso toPair (uncurry snocPath)
  where
    toPair (PSnoc p n) = (p, n)
    toPair PNil        = (PNil, mempty)
{-# INLINE viewBase #-}

viewTop :: Iso' Path (Name, Path)
viewTop = iso toPair (uncurry consPath)
  where
    toPair (PSnoc PNil n) = (n, PNil)
    toPair (PSnoc p n)    = (n', PSnoc p' n)
      where
        (n', p')          = toPair p
    toPair PNil           = (mempty, PNil)
{-# INLINE viewTop #-}

headPath :: Path -> Name
headPath = (^. viewTop . _1)
{-# INLINE headPath #-}

tailPath :: Path -> Path
tailPath = (^. viewTop . _2)
{-# INLINE tailPath #-}

initPath :: Path -> Path
initPath = (^. viewBase . _1)

lastPath :: Path -> Name
lastPath = (^. viewBase . _2)

substPathName :: Name -> Path -> Path
substPathName n p = p & viewBase . _2 .~ n
{-# INLINE substPathName #-}

isPathPrefix :: Path -> Path -> Bool
isPathPrefix p1 p2 =
  p1 == p2
  ||
  ( not (nullPath p2)
    &&
    isPathPrefix p1 (initPath p2)
  )

substPathPrefix :: Path -> Path -> (Path -> Path)
substPathPrefix old'px new'px p0 =
  go p0
  where
    go p1
      | p1 == old'px = new'px
      | nullPath p1  = p1
      | otherwise    = go p2 `snocPath` n2
        where
          (p2, n2) = p1 ^. viewBase


remCommonPathPrefix :: Path -> Path -> (Path, Path)
remCommonPathPrefix p1 p2
  | n1 /= n2 = (p1, p2)
  | nullPath p1' = (p1', p2')
  | nullPath p2' = (p1', p2')
  | otherwise    = remCommonPathPrefix p1' p2'
  where
    (n1, p1') = p1 ^. viewTop
    (n2, p2') = p2 ^. viewTop


editName :: (Text -> Bool) -> (Text -> Text) -> Path -> Path
editName pr ed p =
  p & viewBase . _2 . isoText . isA pr %~ ed

checkAndRemExt :: String -> Path -> Maybe Path
checkAndRemExt ext p
  | ext' `T.isSuffixOf` nm = Just res
  | otherwise              = Nothing
    where
      ext' = ext ^. isoText
      nm   = p ^. viewBase . _2 . isoText
      res  = p &  viewBase . _2 . isoText %~ T.dropEnd (T.length ext')

showPath' :: String -> Path -> String
showPath' acc PNil        = acc
showPath' acc (PSnoc p n) = showPath' ("/" ++ show n ++ acc) p

showPath :: Path -> String
showPath = showPath' ""

quotePath :: Path -> String
quotePath p = '"' : showPath' "\"" p

msgPath :: Path -> Text -> Text
msgPath p msg =
  T.unwords [msg, "\"" <> textFromPath p <> "\""]

checkExtPath :: Text -> Path -> Bool
checkExtPath ext p
  = ext == ext'
    &&
    T.length bn > ln
  where
    ln   = T.length ext
    bn   = (p ^. viewBase . _2) ^. isoText
    ext' = T.toLower . T.takeEnd ln $ bn

----------------------------------------
--
-- instances for Path

deriving instance Functor Path'
deriving instance Eq  n => Eq  (Path' n)
deriving instance Ord n => Ord (Path' n)

instance Foldable Path' where
  foldMap f = foldl (\ r n -> r <> f n) mempty
  {-# INLINE foldMap #-}

  foldl f e = go
    where
      go PNil = e
      go (PSnoc p1 n) = go p1 `f` n
  {-# INLINE foldl #-}

  foldr f e = go e
    where
      go acc PNil = acc
      go acc (PSnoc p1 n) = go (n `f` acc) p1
  {-# INLINE foldr #-}

instance Semigroup Path where
  (<>) = concPath

instance Monoid Path where
  mempty = PNil
  mappend = (<>)
  {-# INLINE mappend #-}
  {-# INLINE mempty #-}

instance IsEmpty Path where
  isempty = nullPath
  {-# INLINE isempty #-}

instance IsoString Path where
  isoString = iso showPath readPath
  {-# INLINE isoString #-}

instance IsoText Path where
  isoText = iso textFromPath textToPath

instance Show Path where
  show = showPath
  {-# INLINE show #-}

instance ToJSON Path where
  toJSON = toJSON . textFromPath
  {-# INLINE toJSON #-}

instance FromJSON Path where
  parseJSON o = textToPath <$> parseJSON o

instance IsString Path where
  fromString = readPath
  {-# INLINE fromString #-}

instance Hashable64 Path where
  hash64Add = hash64Add . showPath

-- ----------------------------------------
