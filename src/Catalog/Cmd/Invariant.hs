module Catalog.Cmd.Invariant
  ( checkImgStore )
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as Seq

import           Control.Monad.Trans.Maybe

-- ----------------------------------------

-- type CmdMB a = MaybeT Cmd a  -- in Catalog.Cmd.Types

-- ----------------------------------------

allCleanupImgRefs :: Cmd ()
allCleanupImgRefs = getRootId >>= cleanupImgRefs

cleanupImgRefs :: ObjId -> Cmd ()
cleanupImgRefs i0 = do
  verbObj i0 "cleanupImgRefs: remove outdated img refs for "

  foldMTU ignoreImg dirA foldRoot colA i0
  where
    -- check all img refs in DIR case
    -- and traverse all subcollection

    dirA go i es0 _ts = do
      -- trcObj i "cleanupImgRefs: process img dir: "

      let es = es0 ^. isoDirEntries
      es' <- filterSeqM (isOK checkDirRef) es

      when (Seq.length es' < Seq.length es) $ do
        warnObj i $
          "cleanupImgRefs: dir entries removed " ++ show (es, es') ++ " in "
        adjustDirEntries (const $ isoDirEntries # es') i

      mapM_ go es'

    -- in COL case check optional img and/or blog ref
    -- and all img refs in the entries list
    -- and recurse into subcollections

    colA go i md im be es = do
      -- trcObj i $ "cleanupImgRefs: process collection "

      im' <- filterMM (isOK checkImgPart) im
      when (im /= im') $ do
        warnObj i $
          "cleanupImgRefs: col img ref " ++ show (im, im') ++ " removed in "
        adjustColImg (const im') i

      be' <- filterMM (isOK checkImgPart) be
      when (be /= be') $ do
        warnObj i $
          "cleanupImgRefs: col blog ref" ++ show (be, be') ++ " removed in "
        adjustColBlog (const be') i

      es' <- filterSeqM (isOK checkColEntry) es
      when (Seq.length es' < Seq.length es) $ do
        warnObj i $
          "cleanupImgRefs: col enties " ++ show (es, es') ++ " removed in "
        adjustColEntries (const es') i

      -- recurse into subcollections
      foldColEntries go i md im be es

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
      (^. nodeVal) <$> (MaybeT $ getTreeAt i)

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
      as <- S.fromList . M.keys <$> use (theImgTree . entries)
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
  cleanupOrphanIds orphanIds
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

    cleanupOrphanIds os
      | S.null os =
          return ()
      | otherwise = do
          trc $ "checkUsedImgObjIds: removing "
                ++ show (S.size os)
                ++ " orphan image ids "
          traverse_ rm os
            where
              rm i = do
                trcObj i "checkUsedImgObjIds: removing "
                rmImgNode i

-- ----------------------------------------

checkUpLinkObjIds :: Cmd ()
checkUpLinkObjIds =
  getRootId >>= allWrongUpLinks >>= showWrongUpLinks
  where
    allWrongUpLinks :: ObjId -> Cmd ObjIds
    allWrongUpLinks =
      foldMTU ignoreImg dirA rootA colA
      where
        dirA go i es ts = do
          s0 <- toObjIds i
                <$>
                traverse getImgParent (es ^. isoDirEntries)
          s1 <- foldDir go i es ts
          return $ s0 <> s1

        rootA go i dir col = do
          s0 <- toObjIds i
                <$>
                traverse getImgParent [i, dir, col]
          s1 <- foldRoot go i dir col
          return $
            s0 <> s1

        colA go i md im be es = do
          s0 <- toObjIds i
                <$>
                traverse getImgParent (es ^.. traverse . theColColRef)

          s1 <- foldColEntries go i md im be es
          return $
            s0 <> s1

        toObjIds :: Foldable t => ObjId -> t ObjId -> ObjIds
        toObjIds i = foldMap neI
          where
            neI j | j == i    = mempty
                  | otherwise = S.singleton j

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
                getTreeAt i >>= maybe (return ()) msg
                where
                  msg n = do
                    warnObj i $
                      "checkUpLinkObjIds: node "
                      ++ show n
                      ++ " contains element not pointing back to "

-- ----------------------------------------
