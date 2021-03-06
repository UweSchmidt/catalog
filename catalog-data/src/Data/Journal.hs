{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Journal where

import Data.ImgTree  ( ColEntries'
                     , DirEntries
                     , ImgParts
                     , ImgRef'
                     )
import Data.MetaData ( MetaData )
import Data.Prim     ( Name
                     , ObjId
                     , Path
                     , Text
                     , TimeStamp
                     )
import Data.History  ( HistoryID )

data Journal' ref = MkIMG           ref Name
                  | MkDIR           ref Name
                  | MkCOL           ref Name
                  | RmObj           ref
                  | AdjImgParts     ref ImgParts
                  | AdjDirEntries   ref DirEntries
                  | AdjMetaData     ref MetaData
                  | AdjPartMetaData Name ref MetaData
                  | AdjColImg       ref (Maybe (ImgRef' ref))
                  | AdjColBlog      ref (Maybe (ImgRef' ref))
                  | AdjColEntries   ref (ColEntries' ref)
                  | SetSyncTime     ref TimeStamp
                  | InitImgStore    Name Name Name
                  | LoadImgStore    FilePath
                  | SaveImgStore    FilePath
                  | SaveBlogText    ref Name Text
                  | NewUndo         HistoryID
                  | DoUndo          HistoryID
                  | DropUndo        HistoryID
                  | NoOp
                  | JSeq (Journal' ref) (Journal' ref)

deriving instance (Show ref) => Show (Journal' ref)
deriving instance Functor     Journal'
deriving instance Foldable    Journal'
deriving instance Traversable Journal'

type Journal  = Journal' ObjId
type JournalP = Journal' Path

-- ----------------------------------------
