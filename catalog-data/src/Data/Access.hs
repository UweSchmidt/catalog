{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Access
  ( AccessRestr(..)
  , Access

  -- access rights
  , all'restr
  , no'restr
  , no'delete
  , no'sort
  , no'write
  , no'user

    -- lenses
  , accessRestr
  , isoAccessRestr
  , isoAccText
  )
where

import           Data.Prim.Prelude
import           Data.Bits           ( bit
                                     , (.|.)
                                     , testBit
                                     , setBit
                                     , clearBit
                                     )
import           Data.Maybe          (mapMaybe)
import qualified Data.Text  as T

-- ----------------------------------------

data AccessRestr = NO'write | NO'delete | NO'sort | NO'user

type Access = Int

-- --------------------

deriving instance Show    AccessRestr
deriving instance Eq      AccessRestr
deriving instance Ord     AccessRestr
deriving instance Enum    AccessRestr
deriving instance Bounded AccessRestr

accessNames :: [Text]
accessNames = map fst accessMap

accessMap :: [(Text, AccessRestr)]
accessMap =
  map (\ a -> (toT a, a)) [minBound .. maxBound]
  where
    toT :: AccessRestr -> Text
    toT = T.pack . map f . show
      where
        f '\'' = '-'
        f c    = toLower c

all'restr
  , no'restr
  , no'delete
  , no'sort
  , no'write
  , no'user :: Access

[no'write, no'delete, no'sort, no'user] = map toA [minBound .. maxBound]
  where
    toA :: AccessRestr -> Access
    toA = bit . fromEnum

all'restr = no'write .|. no'delete .|. no'sort .|. no'user
no'restr  = 0

-- indexed access to a single restriction

accessRestr :: AccessRestr -> Lens' Access Bool
accessRestr r k a =
  (\ b -> ( if b
            then setBit
            else clearBit
          ) a (fromEnum r)
  ) <$>
  k (testBit a (fromEnum r))


isoAccessRestr :: Iso' Access [AccessRestr]
isoAccessRestr = iso toS frS
  where
    toS :: Access -> [AccessRestr]
    toS a = foldr add [] [minBound .. maxBound]
      where
        add r acc
          | a ^. accessRestr r = r : acc
          | otherwise          =     acc

    frS rs = foldl' sb no'restr rs
      where
        sb :: Access -> AccessRestr -> Access
        sb a r = a & accessRestr r .~ True

isoAccText :: Iso' [AccessRestr] Text
isoAccText = iso toT frT
  where
    toT :: [AccessRestr] -> Text
    toT = T.unwords . map (\ r -> accessNames !! fromEnum r)

    frT :: Text -> [AccessRestr]
    frT = mapMaybe (flip lookup accessMap) . T.words
{-# INLINE isoAccText #-}

-- ----------------------------------------
