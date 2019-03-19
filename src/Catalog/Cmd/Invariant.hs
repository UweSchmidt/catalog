module Catalog.Cmd.Invariant
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.MetaData
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import           Control.Monad.Trans.Maybe

-- ----------------------------------------

-- type CmdMB a = MaybeT Cmd a  -- in Catalog.Cmd.Types

-- ----------------------------------------

checkImgStore' :: Cmd ()
checkImgStore' = do
  verbose "checkImgStore: check integrity of the archive"
  allCleanupImgRefs
  checkDeadObjIds
  -- cleanupDeadRefs
  _us <- allUndefRefs
  checkUndefObjIds
  _ps <- allCheckUpLink
  return ()

-- ----------------------------------------

allCleanupImgRefs :: Cmd ()
allCleanupImgRefs = getRootId >>= cleanupImgRefs

cleanupImgRefs :: ObjId -> Cmd ()
cleanupImgRefs i0 = do
  p <- objid2path i0
  verbose $ "cleanupImgRefs: remove outdated img refs for: " ++ show (i0, p)

  foldMT' undefId imgA dirA rootA colA i0
  where
    undefId i
      = warn $ "cleanupImgRefs: undefined obj id ignored: " ++ show i

    -- nothing to do in IMG cases
    imgA _i _pts _md
      = return ()

    -- check all img refs in DIR case
    -- and traverse all subcollection

    dirA go i es0 _ = do
      p <- objid2path i
      -- trc $ "cleanupImgRefs: process img dir: " ++ quotePath p

      let es = es0 ^. isoDirEntries
      es' <- filterM (isOK checkDirRef) es
      when (length es' < length es) $ do
        warn $ "cleanupImgRefs: dir entries removed in: "
               ++ quotePath p ++ ", " ++ show (es, es')
        adjustDirEntries (const $ es' ^. from isoDirEntries) i

      mapM_ go es'

    -- in COL case check optional img and/or blog ref
    -- and all img refs in the entries list
    -- and recurse into subcollections

    colA :: (ObjId -> Cmd ())
         -> ObjId
         -> MetaData -> Maybe ImgRef -> Maybe ImgRef -> [ColEntry]
         -> Cmd ()
    colA go i _md im be es = do
      p <- objid2path i
      -- trc $ "cleanupImgRefs: process collection: " ++ quotePath p

      im' <- filterMM (isOK checkImgPart) im
      when (im /= im') $ do
        warn $ "cleanupImgRefs: col img ref removed in: "
               ++ quotePath p ++ ", " ++ show (im, im')
        adjustColImg (const im') i

      be' <- filterMM (isOK checkImgPart) be
      when (be /= be') $ do
        warn $ "cleanupImgRefs: col blog ref removed in: "
               ++ quotePath p ++ ", " ++ show (be, be')
        adjustColBlog (const be') i

      es' <- filterM (isOK checkColEntry) es
      when (length es' < length es) $ do
        warn $ "cleanupImgRefs: col enties removed in: "
               ++ quotePath p ++ ", " ++ show (es, es')
        adjustColEntries (const es') i

      mapM_ (colEntry' (\ _ -> return ()) go) es'

    -- in ROOT case: traverse COL and DIR hierachies
    rootA go _i dir col = go dir >> go col

    -- the little helpers, a lot of Maybe's in command results
    -- so the MaybeT transformer helps eliminating case expression

    filterMM :: (a -> Cmd Bool) -> Maybe a -> Cmd (Maybe a)
    filterMM _ Nothing = return Nothing
    filterMM f r@(Just x) = do
      ok <- f x
      return $
        if ok then r else Nothing

    -- kill the case-Nothing-Just code with monad transformer MaybeT
    -- check whether the ref i exists and points to an IMG value

    checkRef :: ObjId -> CmdMB ImgNode
    checkRef i =
      (^. nodeVal) <$> (MaybeT $ getTree (entryAt i))

    checkDirRef :: ObjId -> CmdMB ImgNode
    checkDirRef i = do
      n <- checkRef i
      if isIMG n || isDIR n
        then return n
        else empty

    checkImgRef :: ObjId -> CmdMB ImgNode
    checkImgRef i = do
      n <- checkRef i
      if isIMG n
        then return n
        else empty

    -- check whether both the ref and the part in an ImgRef exist
    checkImgPart :: ImgRef -> CmdMB ImgNode
    checkImgPart (ImgRef i nm) = do
      n <- checkImgRef i
      _ <- return $ n ^? theParts . isoImgPartsMap . at nm
      return n

    -- check a ColEntry ref for existence
    -- only the ImgRef's are checked, not the ColRef's
    checkColEntry :: ColEntry -> CmdMB ColEntry
    checkColEntry ce =
      colEntry' imgRef colRef ce
      where
        colRef _ =
          return ce
        imgRef ir =
          checkImgPart ir >> return ce

    isOK :: (b -> CmdMB a) -> b -> Cmd Bool
    isOK cmd i =
      isJust <$> runMaybeT (cmd i)

-- ----------------------------------------

allUndefRefs :: Cmd ObjIds
allUndefRefs = getRootId >>= undefRefs

undefRefs :: ObjId -> Cmd ObjIds
undefRefs i0 = do
  p <- objid2path i0
  verbose $ "undefRefs: search undefined refs for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  verbose $ "undefRefs: to be cleaned up: " ++ show (S.toList s)
  return s
  where
    undefId i
      = do warn $ "undefRefs: undefined obj id found: " ++ show i
           return $ S.singleton i

    imgA _i _pts _md
      = return S.empty

    dirA go i es _ = do
      s <- S.unions <$> mapM go (es ^. isoDirEntries)
      warnU i s

    colA go i _md im be es = do
      s1 <- mapMb im
      s2 <- mapMb be
      s3 <- mapM (go . (^. theColObjId)) (filter isColColRef es)
      warnU i $ S.unions (s1 : s2 : s3)
      where
        mapMb =
          maybe (return S.empty) (go . (\(ImgRef i' _name) -> i'))

    rootA go i dir col = do
      s <- S.union <$> go dir <*> go col
      warnU i s

    warnU i s
      | S.null s =
          return s
      | otherwise = do
          p <- objid2path i
          warn $ "undefRefs: undefined refs found: " ++ show (p, S.toList s)
          return s

-- ----------------------------------------

allDefinedRefs :: Cmd ObjIds
allDefinedRefs = getRootId >>= definedRefs

definedRefs :: ObjId -> Cmd ObjIds
definedRefs i0 = do
  p <- objid2path i0
  verbose $ "definedRefs: compute defined refs for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  verbose $ "definedRefs: refs found: " ++ show (S.size s)
  return s
  where
    undefId i = do
      warn $ "definedRefs: undefined obj id found: " ++ show i
      return S.empty

    imgA i _pts _md =
      return s0
        where
          s0 = S.singleton i

    dirA go i es _ = do
      s1 <- fold <$> traverse go (es ^. isoDirEntries)
      return (s0 <> s1)
        where
          s0 = S.singleton i

    colA go i _md im be es = do
      s1 <- fold <$> traverse (go . _iref) im
      s2 <- fold <$> traverse (go . _iref) be
      s3 <- fold <$> traverse (go . (^. theColObjId)) (filter isColColRef es)
      return (mconcat [s0, s1, s2, s3])
        where
          s0 = S.singleton i

    rootA go i dir col = do
      s1 <- go dir
      s2 <- go col
      return (mconcat [s0, s1, s2])
        where
          s0 = S.singleton i

-- ----------------------------------------

allDeadRefs :: Cmd ObjIds
allDeadRefs = do
  us <- allDefinedRefs
  as <- (S.fromList . M.keys) <$> getTree entries
  return $ as `S.difference` us

cleanupDeadRefs :: Cmd ()
cleanupDeadRefs = do
  ds <- allDeadRefs
  unless (S.null ds) $ do
    warn $ "cleanupDeadRefs: removing read refs: " ++ show (S.toList ds)
    theImgTree . entries %= rmDeadRefs ds
  where
    rmDeadRefs ds m = m `M.difference` M.fromSet (const ()) ds

-- ----------------------------------------

allCheckUpLink :: Cmd ObjIds
allCheckUpLink = getRootId >>= checkUpLink

checkUpLink :: ObjId -> Cmd ObjIds
checkUpLink i0 = do
  p <- objid2path i0
  verbose $ "checkUpLink: check uplinks for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  unless (S.null s) $
    warn $ "checkUpLink: wrong uplinks found: " ++ show (S.toList s)
  return s
  where
    undefId i = do
      warn $ "checkUpLink: undefined obj id found: " ++ show i
      return S.empty

    imgA _i _pts _md =
      return S.empty

    dirA go i es _ = do
      s0 <- S.fromList . filter (/= i)
            <$>
            traverse getImgParent (es ^. isoDirEntries)

      unless (S.null s0) $ do
        p <- objid2path i
        warn $ "checkUpLink: uplink(s) wrong in DIR node: " ++ show (p, s0)

      s1 <- fold <$> traverse go (es ^. isoDirEntries)
      return $ s0 `S.union` s1

    colA go i _md _im _be es = do
      s0 <- S.fromList . filter (/= i)
            <$>
            traverse getImgParent esc

      unless (S.null s0) $ do
        p <- objid2path i
        warn $ "checkUpLink: uplink(s) wrong in COL node: " ++ show (p, s0)

      s1 <- fold <$> traverse go esc
      return $ s0 `S.union` s1
        where
          esc = es ^.. traverse . theColColRef

    rootA go i dir col = do
      s0 <- S.fromList . filter (/= i)
            <$>
            traverse getImgParent [i, dir, col]

      unless (S.null s0) $ do
        warn $ "checkUpLink: uplink(s) wrong in ROOT node: " ++ show i

      s1 <- go dir
      s2 <- go col
      return $ s0 `S.union` s1 `S.union` s2

-- ----------------------------------------
--
-- new check operations
--
-- ----------------------------------------

checkImgStore :: Cmd ()
checkImgStore = do
  verbose "checkImgStore: check integrity of the archive"
  allCleanupImgRefs

  checkDeadObjIds
  checkUndefObjIds
  checkUsedImgObjIds
  checkUpLinkObjIds

  verbose "checkImgStore: all checks done"

-- ----------------------------------------

checkUndefObjIds :: Cmd ()
checkUndefObjIds =
  getRootId >>= allUndefObjIds >>= showUndefObjIds
  where
    showUndefObjIds os
      | S.null os =
          trc "checkUndefObjIds: no undefined ObjIds found"
      | otherwise =
          warn $  "checkUndefObjIds: inconsistent catalog\n"
                  ++ show n ++ " undefined ObjIds found\n"
                  ++ show (toList os)
      where
        n = S.size os

-- ----------------------------------------

checkDeadObjIds :: Cmd ()
checkDeadObjIds = do
  dos <- getRootId >>= allDeadObjIds
  showDeadObjIds dos
  cleanupDeadObjIds dos
  where

    allDeadObjIds t = do
      us <- allObjIds t
      as <- S.fromList . M.keys <$> getTree entries
      return $ as `S.difference` us

    showDeadObjIds os
      | S.null os =
          trc "checkDeadObjIds: no dead ObjIds found"
      | otherwise =
          warn $  "checkDeadObjIds: "
                  ++ show n ++ " undefined ObjIds found\n"
                  ++ show (toList os)
      where
        n = S.size os

    cleanupDeadObjIds ds = do
      unless (S.null ds) $ do
        warn $ "checkDeadObjIds: removing dead ObjIds"
        theImgTree . entries %= rmDeadRefs ds
      where
        rmDeadRefs os m = m `M.difference` M.fromSet (const ()) os

-- ----------------------------------------

checkUsedImgObjIds :: Cmd ()
checkUsedImgObjIds = do
  ds <- getRootImgDirId >>= allImgObjIds
  cs <- getRootImgColId >>= allImgObjIds
  let orphanIds = ds `S.difference` cs
  let undefIds  = cs `S.difference` ds
  trc $ "checkUsedImgObjIds: image  ids in dirs: " ++ show (S.size ds)
  trc $ "checkUsedImgObjIds: image  ids in cols: " ++ show (S.size cs)
  trc $ "checkUsedImgObjIds: orphan ids in dirs: " ++ show (S.size orphanIds)
  trc $ "checkUsedImgObjIds: undef  ids in cols: " ++ show (S.size undefIds)
  showRes orphanIds undefIds
  where
    showRes ds cs = do
      case (nds, ncs) of
        (True,  True ) ->
          trc "checkUsedImgObjIds: image refs in collections and dir are the same"
        (False, True ) ->
          warn $ "checkUsedImgObjIds: orphan image ids: " ++ show (toList ds)
        (True,  False) ->
          warn $ "checkUsedImgObjIds: undefined image ids: " ++ show (toList cs)
        (False, False) -> do
          showRes mempty cs
          showRes ds mempty
      where
        nds = S.null ds
        ncs = S.null cs

-- ----------------------------------------

checkUpLinkObjIds :: Cmd ()
checkUpLinkObjIds = do
  getRootId >>= allWrongUpLinks >>= showWrongUpLinks
  where
    allWrongUpLinks :: ObjId -> Cmd ObjIds
    allWrongUpLinks =
      foldMT' undefId imgA dirA rootA colA
      where
        undefId _i =
          return mempty

        imgA _i _pts _md =
          return mempty

        dirA go i es _ = do
          s0 <- S.fromList . filter (/= i)
                <$>
                traverse getImgParent (es ^. isoDirEntries)
          s1 <- fold <$> traverse go  (es ^. isoDirEntries)
          return $ s0 <> s1

        rootA go i dir col = do
          s0 <- S.fromList . filter (/= i)
                <$>
                traverse getImgParent [i, dir, col]
          s1 <- go dir
          s2 <- go col
          return $ s0 <> s1 <> s2

        colA go i _md _im _be es = do
          s0 <- S.fromList . filter (/= i)
                <$>
                traverse getImgParent esc

          s1 <- fold <$> traverse go esc
          return $ s0 <> s1
            where
              esc = es ^.. traverse . theColColRef

    showWrongUpLinks :: ObjIds -> Cmd ()
    showWrongUpLinks os
      | S.null os =
          trc "checkUpLinkObjIds: all uplinks o.k."
      | otherwise = do
          warn $  "checkUpLinkObjIds: inconsistent catalog\n"
                  ++ show (S.size os) ++ " undefined ObjIds found\n"
                  ++ show (toList os)
          foldObjIds showObj os
            where
              showObj i =
                getTree (entryAt i) >>= maybe (return ()) msg
                where
                  msg n = do
                    p <- objid2path i
                    warn $ "checkUpLinkObjIds:"
                           ++ "node contains element not pointing back to objid\n"
                           ++  " objid = " ++ show i
                           ++ ", path = "  ++ show p
                           ++ ", node = "  ++ show n

-- ----------------------------------------
