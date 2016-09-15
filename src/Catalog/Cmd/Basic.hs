{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd.Basic
where

import           Catalog.Cmd.Types
import           Catalog.Journal
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

-- simple monadic ops

dt :: Cmd ImgTree
dt = use theImgTree
{-# INLINE dt #-}

getTree :: Getting a ImgTree a -> Cmd a
getTree l = use (theImgTree . l)
{-# INLINE getTree #-}

getImgName :: ObjId -> Cmd Name
getImgName i = getTree (theNode i . nodeName)
{-# INLINE getImgName #-}

getImgParent :: ObjId -> Cmd ObjId
getImgParent i = getTree (theNode i . parentRef)
{-# INLINE getImgParent #-}

getImgVal :: ObjId -> Cmd ImgNode
getImgVal i = getTree (theNode i . nodeVal)
{-# INLINE getImgVal #-}

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = getTree (theNode i . nodeVal . l)
{-# INLINE getImgVals #-}

getImgSubDirs :: DirEntries -> Cmd [ObjId]
getImgSubDirs es =
  filterM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoDirEntries)

-- ----------------------------------------

getRootId :: Cmd ObjId
getRootId = getTree rootRef
{-# INLINE getRootId #-}

getRootImgDirId :: Cmd ObjId
getRootImgDirId = do
  ri <- getRootId
  getImgVals ri theRootImgDir
{-# INLINE getRootImgDirId #-}

getRootImgColId :: Cmd ObjId
getRootImgColId = do
  ri <- getRootId
  getImgVals ri theRootImgCol
{-# INLINE getRootImgColId #-}

existsObjId :: ObjId -> Cmd Bool
existsObjId i =
  isJust <$> getTree (entryAt i)
{-# INLINE existsObjId #-}

lookupByName :: Name -> ObjId -> Cmd (Maybe (ObjId, ImgNode))
lookupByName n i = do
  p <- (`snocPath` n) <$> objid2path i
  lookupByPath p

lookupByPath :: Path -> Cmd (Maybe (ObjId, ImgNode))
lookupByPath p = lookupImgPath p <$> dt
{-# INLINE lookupByPath #-}

-- save lookup by path

getIdNode :: String -> Path -> Cmd (ObjId, ImgNode)
getIdNode msg p = do
  mv <- lookupImgPath p <$> dt
  case mv of
    Nothing ->
      abort $ msg ++ " " ++ show (show p)
    Just res ->
      return res

getIdNode' :: Path -> Cmd (ObjId, ImgNode)
getIdNode' p =
  getIdNode ("cant' find entry for path " ++ show (p ^. isoString)) p

-- check path not there

notTherePath :: String -> Path -> Cmd ()
notTherePath msg p = do
  exists <- isJust <$> lookupByPath p
  when exists $
    abort $ msg ++ " " ++ show (show p)

-- ----------------------------------------

-- | ref to path
objid2path :: ObjId -> Cmd Path
objid2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
objid2type :: ObjId -> Cmd String
objid2type i = getImgVal i >>= go
  where
    go e = return $ concat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol     . to (const "COL")
            )

objid2contNames :: ObjId -> Cmd [Name]
objid2contNames i = getImgVal i >>= go
  where
    go e
      | isIMG e =
          return (e ^. theParts . isoImgParts . traverse . theImgName . to (:[]))

      | isDIR e =
          traverse getImgName (e ^. theDirEntries . isoDirEntries)

      | isROOT e = let (i1, i2) = e ^. theImgRoot in do
          n1 <- getImgName i1
          n2 <- getImgName i2
          return [n1, n2]

      | otherwise =
          return []

-- ----------------------------------------
--

-- | convert an image path to a file system path
toFilePath :: Path -> Cmd FilePath
toFilePath p = do
  mp <- use theMountPath
  return $ mp ++ tailPath p ^. isoString

-- | convert a file system path to an image path
fromFilePath :: FilePath -> Cmd Path
fromFilePath f = do
  mp <- use theMountPath
  unless (mp `isPrefixOf` f) $
    abort $ "fromFilePath: not a legal image path " ++ show f
  r' <- getTree rootRef >>= getImgName
  return $ consPath r' (readPath $ drop (length mp) f)

-- ----------------------------------------
--
-- smart constructors

mkImg' :: (ObjId -> Name -> Journal) -> (ImgNode -> Bool) -> ImgNode -> ObjId -> Name -> Cmd ObjId
mkImg' mkj isN v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkNode isN n i v t
      theImgTree .= t'
      journalChange $ mkj i n
      return d

mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' MkDIR isDIR emptyImgDir
{-# INLINE mkImgDir #-}

mkImgCol :: ObjId -> Name -> Cmd ObjId
mkImgCol = mkImg' MkCOL isCOL emptyImgCol
{-# INLINE mkImgCol #-}

mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' MkIMG isDIR emptyImg
{-# INLINE mkImg #-}

rmImgNode :: ObjId -> Cmd ()
rmImgNode i = dt >>= go
  where
    go t = do
      t' <- liftE $ removeImgNode i t
      theImgTree .= t'
      journalChange $ RmObj i

-- ----------------------------------------
--
-- simple "file system" ops

-- create a new empty subcollection

mkCollection :: Path -> Cmd ObjId
mkCollection target'path = do
  -- parent exists
  (parent'id, parent'node) <- getIdNode "mkCollection: parent doesn't exist" parent'path

  -- parent is a collection
  -- TODO exists check
  unless (isCOL parent'node) $
    abort $ "mkCollection: parent isn't a collection " ++ show (show parent'path)

  -- check collection does not yet exist
  notTherePath "mkCollection: target collection already exists" target'path

  -- create a new empty collection and append it to the parent collection
  col'id <- mkImgCol parent'id target'name
  adjustColEntries (++ [mkColColRef col'id]) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------
--
-- basic modification commands

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> Cmd ()
adjustImg = adjustNodeVal AdjImgParts theParts

adjustDirEntries :: (DirEntries -> DirEntries) -> ObjId -> Cmd ()
adjustDirEntries = adjustNodeVal AdjDirEntries theDirEntries

adjustMetaData :: (MetaData -> MetaData) -> ObjId -> Cmd ()
adjustMetaData = adjustNodeVal AdjMetaData theColMetaData

adjustColImg :: (Maybe (ObjId, Name) -> Maybe (ObjId, Name)) -> ObjId -> Cmd ()
adjustColImg = adjustNodeVal AdjColImg theColImg

adjustColBlog :: (Maybe (ObjId, Name) -> Maybe (ObjId, Name)) -> ObjId -> Cmd ()
adjustColBlog = adjustNodeVal AdjColBlog theColBlog

adjustColEntries :: ([ColEntry] -> [ColEntry]) -> ObjId -> Cmd ()
adjustColEntries = adjustNodeVal AdjColEntries theColEntries

delColEntry :: Int -> ObjId -> Cmd ()
delColEntry pos = adjustColEntries delEntry
  where
    delEntry cs
      | pos >= 0 && pos < length cs =
          let (xs, ys) = splitAt pos cs in xs ++ drop 1 ys
      | otherwise = cs

setSyncTime :: ObjId -> Cmd ()
setSyncTime i = do
  t <- now
  adjustNodeVal SetSyncTime theSyncTime (const t) i

adjustNodeVal :: Show a =>
                 (ObjId -> a -> Journal) ->
                 Traversal' ImgNode a -> (a -> a) -> ObjId -> Cmd ()
adjustNodeVal mkj theComp f i = do
  theImgTree . theNodeVal i . theComp %= f
  -- journal the changed result
  dt >>= journalAdjust
  where
    journalAdjust t =
      case t ^.. theNodeVal i . theComp of
        -- the expected case
        [new'v] ->
          journalChange $ mkj i new'v

        -- the error cases
        [] ->
          warn $ "adjustNodeVal: nothing changed, component to be modified does not exist in " ++ name'i

        -- the critical case
        vs ->
          warn $ "adjustNodeVal: mulitple components have been changed in " ++ name'i ++ ": " ++ show vs
      where
        name'i = show $ i ^. isoString

-- ----------------------------------------
--
-- search, sort and merge ops for collections

findAllColEntries :: (ColEntry -> Cmd Bool) -> ObjId -> Cmd [(Int, ColEntry)]
findAllColEntries p i = do
  es <- getImgVals i theColEntries
  filterM (p . snd) $ zip [0..] es

findFstColEntry  :: (ColEntry -> Cmd Bool) -> ObjId -> Cmd (Maybe (Int, ColEntry))
findFstColEntry p i = listToMaybe <$> findAllColEntries p i


sortColEntries :: (ColEntry -> Cmd a) ->
                  (a -> a -> Ordering) ->
                  [ColEntry] -> Cmd [ColEntry]
sortColEntries getVal cmpVal es = do
  map fst . sortBy (cmpVal `on` snd) <$> mapM mkC es
  where
    -- mkC :: ColEntry -> Cmd (ColEntry, a)
    mkC ce = do
      v <- getVal ce
      return (ce, v)


-- merge old an new entries
-- old entries are removed from list of new entries
-- the remaining new entries are appended

mergeColEntries :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries es1 es2 =
  es1 ++ filter (`notIn` map (^. theColObjId) es1) es2
  where
    notIn e' es' = (e' ^. theColObjId) `notElem` es'

-- a faster version, where the result is unordered
-- duplicates are removed, useful when the list of entries
-- is sorted afterwards
mergeColEntries' :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries' ns os =
  (ns ++ os) ^. (from isoDirEntries) . isoDirEntries

-- ----------------------------------------
--
-- basic Cmd combinators

fromJustCmd :: String -> Maybe a -> Cmd a
fromJustCmd _   (Just x) = return x
fromJustCmd msg Nothing  = abort msg

liftE :: Except String a -> Cmd a
liftE cmd =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res -> return res

catchAll :: Cmd () -> Cmd ()
catchAll c =
  c `catchError` (\ e -> warn $ "catchAll: error caught: " ++ show e)

runDry :: String -> Cmd () -> Cmd ()
runDry msg cmd = do
  dry <- view envDryRun
  if dry
    then do
      logg (^. envDryRun) "dry-run" msg
    else do
      verbose $ msg
      cmd

-- ----------------------------------------
--
-- trace commands

trcObj :: ObjId -> String -> Cmd ()
trcObj r msg = dt >>= \ t ->
  trc $ msg ++ " " ++ show (refPath r t)

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

journalChange :: Journal -> Cmd ()
journalChange j = do
  logg (^. envJournal) "journal" (show j)

-- ----------------------------------------
