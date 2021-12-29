module Data.Journal where

import Data.ImgTree
       ( ColEntries'
       , DirEntries
       , ImgParts
       , ImgRef'
       )
import Data.MetaData
       ( MetaData )

import Data.Prim
       ( Name
       , ObjId
       , Path
       , Text
       , TimeStamp
       )
import Data.History
       ( HistoryID )

import Data.Aeson
       ( ToJSON(..)
       , (.=)
       , object
       )

-- ----------------------------------------

type Journal  = Journal' ObjId
type JournalP = Journal' Path

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

instance (ToJSON ref) => ToJSON (Journal' ref) where
  toJSON j = case j of
    MkIMG p n
      -> object ["cmd"   .= ("MkIMG" :: Text)
                , "path" .= p
                , "name" .= n
                ]
    MkDIR p n
      -> object ["cmd"   .= ("MkDIR" :: Text)
                , "path" .= p
                , "name" .= n
                ]
    MkCOL p n
      -> object ["cmd"   .= ("MkCOL" :: Text)
                , "path" .= p
                , "name" .= n]
    RmObj p
      -> object ["cmd"  .= ("RmObj" :: Text)
                , "path" .= p
                ]
    AdjImgParts p ps
      -> object [ "cmd"   .= ("AdjParts" :: Text)
                , "path"  .= p
                , "parts" .= ps
                ]
    AdjDirEntries p ds
      -> object [ "cmd"     .= ("AdjDirEntries" :: Text)
                , "path"    .= p
                , "entries" .= ds
                ]
    AdjMetaData p md
      -> object [ "cmd"  .= ("AdjMetaData" :: Text)
                , "path" .= p
                , "md" .= md]
    AdjPartMetaData n p md
      -> object [ "cmd"  .= ("AdjPartMetaData" :: Text)
                , "name" .= n
                , "path" .= p
                , "md"   .= md
                ]
    AdjColImg p i
      -> object [ "cmd"  .= ("AdjColImg" :: Text)
                , "path" .= p
                , "iref" .= i
                ]
    AdjColBlog p i
      -> object [ "cmd"  .= ("AdjColBlog" :: Text)
                , "path" .= p
                , "iref" .= i
                ]
    AdjColEntries p es
      -> object [ "cmd"     .= ("AdjColEntries" :: Text)
                , "path"    .= p
                , "entries" .= es
                ]
    SetSyncTime p t
      -> object [ "cmd"       .= ("SetSyncTime" :: Text)
                , "path"      .= p
                , "timestamp" .= t
                ]
    InitImgStore n1 n2 n3
      -> object [ "cmd"   .= ("InitImgStore" :: Text)
                , "name1" .= n1
                , "name2" .= n2
                , "name3" .= n3
                ]
    LoadImgStore fp
      -> object [ "cmd"  .= ("LoadImgStore" :: Text)
                , "file" .= fp
                ]
    SaveImgStore fp
      -> object [ "cmd"  .= ("SaveImgStore" :: Text)
                , "file" .= fp
                ]
    SaveBlogText p n t
      -> object [ "cmd"  .= ("SaveBlogText" :: Text)
                , "path" .= p
                , "name" .= n
                , "text" .= t
                ]
    NewUndo h
      -> object [ "cmd" .= ("NewUndo" :: Text)
                , "hid" .= h
                ]
    DoUndo h
      -> object [ "cmd" .= ("DoUndo" :: Text)
                , "hid" .= h
                ]
    DropUndo h
      -> object [ "cmd" .= ("DropUndo" :: Text)
                , "hid" .= h
                ]
    NoOp
      -> object [ "cmd" .= ("NoOp" :: Text) ]

    JSeq j1 j2
      -> object [ "cmd" .= ("JSeq" :: Text)
                , "j1"  .= j1
                , "j2"  .= j2
                ]

-- ----------------------------------------
