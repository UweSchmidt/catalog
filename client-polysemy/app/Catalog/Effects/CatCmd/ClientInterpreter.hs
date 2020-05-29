{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

module Catalog.Effects.CatCmd.ClientInterpreter
  ( -- * Interpreter
    evalClientCatCmd
  )
where

-- polysemy
import Polysemy

-- polysemy-tools
import Polysemy.HttpRequest.SimpleRequests
       ( HttpEffects
       , getReq
       , argJSONReq
       , simpleJSONReq
       )

-- catalog
import Data.Prim

-- catalog-polysemy
import Catalog.Effects.CatCmd

------------------------------------------------------------------------------

evalClientCatCmd :: HttpEffects r => InterpreterFor CatCmd r
evalClientCatCmd =
  interpret $
  \ c -> case c of

    -- JSON modifying commands
    SaveBlogSource pos t p ->
      paramJSONmodify "saveblogsource" p (pos, t)
    ChangeWriteProtected ixs ro p ->
      paramJSONmodify "changeWriteProtected" p (ixs, ro)
    SortCollection ixs p ->
      paramJSONmodify "sort" p ixs
    RemoveFromCollection ixs p ->
      paramJSONmodify "removeFromCollection" p ixs
    CopyToCollection ixs dst p ->
      paramJSONmodify "copyToCollection" p (ixs, dst)
    MoveToCollection ixs dst p ->
      paramJSONmodify "moveToCollection" p (ixs, dst)
    SetCollectionImg sPath pos p ->
      paramJSONmodify "colimg" p (sPath, pos)
    SetCollectionBlog sPath pos p ->
      paramJSONmodify "colblog" p (sPath, pos)
    NewCollection nm p ->
      paramJSONmodify "newcol" p nm
    RenameCollection nm p ->
      paramJSONmodify "renamecol" p nm
    SetMetaData ixs md p ->
      paramJSONmodify "setMetaData" p (ixs, md)
    SetMetaData1 pos md p ->
      paramJSONmodify "setMetaData1" p (pos, md)
    SetRating ixs r p ->
      paramJSONmodify "setRating" p (ixs, r)
    SetRating1 pos r p ->
      paramJSONmodify "setRating1" p (pos, r)
    Snapshot t p ->
      paramJSONmodify "snapshot" p t
    SyncCollection p ->
      simpleJSONmodify "syncCol" p
    SyncExif p ->
      simpleJSONmodify "syncExif" p
    NewSubCollections p ->
      simpleJSONmodify "newSubCols" p
    UpdateCheckSum cs n p ->
      paramJSONmodify "updateCheckSum" p (cs, n)
    UpdateTimeStamp ts n p ->
      paramJSONmodify "updateTimeStamp" p (ts, n)

    -- JSON reading commands
    TheEntry p ->
      simpleJSONget "collection" p   -- TODO: rename to entry
    IsWriteable p ->                 -- but: this must also be changed in JavaScript
      simpleJSONget "isWritable" p
    IsRemovable p ->
      simpleJSONget "isRemovable" p
    IsSortable p ->
      simpleJSONget "isSortable" p
    IsCollection p ->
      simpleJSONget "isCollection" p
    TheBlogContents pos p ->
      paramJSONget "blogcontents" p pos
    TheBlogSource pos p ->
      paramJSONget "blogsource" p pos
    TheMetaData pos p ->
      paramJSONget "metadata" p pos
    TheRating pos p ->
      paramJSONget "rating" p pos
    TheRatings p ->
      simpleJSONget "ratings" p
    CheckImgPart onlyUpdate n p ->
      paramJSONget "checkimgpart" p (onlyUpdate, n)

    -- image and HTML page requests
    StaticFile _dp bn ->
      getReq bn
    JpgImgCopy rt geo p ->
      basicDocReq ".jpg" rt geo p
    HtmlPage rt geo p ->
      basicDocReq ".html" rt geo p

------------------------------------------------------------------------------

simpleJSONget :: (FromJSON a, HttpEffects r)
              => Name -> Path -> Sem r a
simpleJSONget nm p =
  simpleJSONReq $ buildPath "get" nm p

simpleJSONmodify :: (FromJSON a, HttpEffects r)
                 => Name -> Path -> Sem r a
simpleJSONmodify nm p =
  simpleJSONReq $ buildPath "modify" nm p

paramJSONget ::  (FromJSON b, ToJSON a, HttpEffects r)
             => Name -> Path -> a -> Sem r b
paramJSONget nm p v =
  argJSONReq v $ buildPath "get" nm p

paramJSONmodify ::  (FromJSON b, ToJSON a, HttpEffects r)
                => Name -> Path -> a -> Sem r b
paramJSONmodify nm p v =
  argJSONReq v $ buildPath "modify" nm p

buildPath :: Name -> Name -> Path -> Text
buildPath mn cn p =
  (mn `consPath` cn `consPath` p) ^. isoText

basicDocReq :: HttpEffects r
            => Text -> ReqType -> Geo -> Path
            -> Sem r LazyByteString
basicDocReq ext rt geo path0 =
  getReq (path' <> ext)
  where
    path' = ("docs" `consPath` img' `consPath` geo' `consPath` path0) ^. isoText
    img'  = rt   ^. isoString . from isoString
    geo'  = geo  ^. isoString . from isoString

-- ----------------------------------------
