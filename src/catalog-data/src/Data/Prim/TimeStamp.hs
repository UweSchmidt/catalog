module Data.Prim.TimeStamp
       ( TimeStamp
       , now            -- TODO mv now into extra module
       , fsTimeStamp
       , isoEpochTime
       , formatTimeStamp
       , iso8601TimeStamp
       , formatTimeStamp'
       , timeStampToText
       )
where

import Control.Monad.IO.Class
       ( MonadIO
       , liftIO
       )

import Data.Prim.Prelude
       ( MonadPlus(mzero)
       , Text
       , readMaybe
       , (^.)
       , to
       , view
       , iso
       , (#)
       , Iso'
       , IsEmpty(..)
       , IsoString(..)
       , IsoText(..)
       , FromJSON(parseJSON)
       , ToJSON(toJSON)
       )

import System.Posix
       ( FileStatus )

import Data.Time.Clock.POSIX
       ( posixSecondsToUTCTime )

import Data.Time.Format
       ( defaultTimeLocale
       , formatTime
       )

import qualified Data.Aeson   as J
import qualified System.Posix as X

-- ----------------------------------------

newtype TimeStamp = TS X.EpochTime

isoEpochTime :: Iso' TimeStamp X.EpochTime
isoEpochTime = iso (\ (TS et) -> et) TS
{-# INLINE isoEpochTime #-}

deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance IsoString TimeStamp where
  isoString = iso
              (^. isoEpochTime . to show)
              (maybe mempty TS . readMaybe)
  {-# INLINE isoString #-}

instance IsoText TimeStamp where
  isoText = isoString . isoText

instance Semigroup TimeStamp where
  (<>) = max

instance Monoid TimeStamp where
  mempty  = zeroTimeStamp
  mappend = (<>)

instance IsEmpty TimeStamp where
  isempty = (== zeroTimeStamp)
  {-# INLINE isempty #-}

instance ToJSON TimeStamp where
  toJSON = toJSON . view isoString -- (s ->) is an instance of MonadReader
  {-# INLINE toJSON #-}

instance FromJSON TimeStamp where
  parseJSON (J.String t) =
    return $ isoString # (t ^. isoString) -- conv: Text -> String -> TimeStamp

  parseJSON _ =
    mzero

zeroTimeStamp :: TimeStamp
zeroTimeStamp = TS 0
{-# INLINE zeroTimeStamp #-}

-- TODO: move IO actions into extra module

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

fsTimeStamp :: FileStatus -> TimeStamp
fsTimeStamp = TS . X.modificationTime
{-# INLINE fsTimeStamp #-}

formatTimeStamp :: TimeStamp -> String
formatTimeStamp = formatTimeStamp' "%Y-%m-%d %H:%M:%S"

iso8601TimeStamp :: TimeStamp -> String
iso8601TimeStamp = formatTimeStamp' "%Y-%m-%dT%H:%M:%S"

formatTimeStamp' :: String -> TimeStamp -> String
formatTimeStamp' fmt (TS t) =
  formatTime defaultTimeLocale fmt
  . posixSecondsToUTCTime
  . fromIntegral
  $ secs
  where
    secs :: Int
    secs = round . toRational $ t

timeStampToText :: TimeStamp -> Text
timeStampToText ts = formatTimeStamp ts ^. isoText

-- ----------------------------------------
