{-# LANGUAGE
    ConstraintKinds,
    DataKinds,
    FlexibleContexts,
    GADTs,
    OverloadedStrings,
    PolyKinds,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    TypeOperators,
    TypeFamilies
#-} -- default extensions (only for emacs)

------------------------------------------------------------------------------

module Catalog.Invariant
  ( checkImgStore )
where

import Catalog.Effects
import Catalog.Fold
import Catalog.ImgTreeAccess
import Catalog.ImgTreeModify
import Catalog.Logging

import Data.ImageStore
import Data.ImgTree
import Data.Prim

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as Seq

-- ----------------------------------------

allCleanupImgRefs :: SemISEJL r ()
allCleanupImgRefs = getRootId >>= cleanupImgRefs

cleanupImgRefs :: ObjId -> SemISEJL r ()
cleanupImgRefs i0 = do
  verb'Obj i0 "cleanupImgRefs: remove outdated img refs for "

  foldMTU ignoreImg dirA foldRoot colA i0
  where
    -- check all img refs in DIR case
    -- and traverse all subcollection

    dirA go i es0 _ts = do
      -- trcObj i "cleanupImgRefs: process img dir: "

      let es = es0 ^. isoDirEntries
      es' <- filterSeqM (isOK checkDirRef) es

      when (Seq.length es' < Seq.length es) $ do
        warn'Obj i $
          "cleanupImgRefs: dir entries removed "
          <> toText (es, es')
          <> " in "
        adjustDirEntries (const $ isoDirEntries # es') i

      traverse_ go es'

    -- in COL case check optional img and/or blog ref
    -- and all img refs in the entries list
    -- and recurse into subcollections

    colA go i md im be es = do
      -- trcObj i $ "cleanupImgRefs: process collection "

      im' <- filterMB (isOK checkImgPart) (pure im)
      when (im /= im') $ do
        warn'Obj i $
          "cleanupImgRefs: col img ref "
          <> toText (im, im')
          <> " removed in "
        adjustColImg (const im') i

      be' <- filterMB (isOK checkImgPart) (pure be)
      when (be /= be') $ do
        warn'Obj i $
          "cleanupImgRefs: col blog ref"
          <> toText (be, be')
          <> " removed in "
        adjustColBlog (const be') i

      es' <- filterSeqM (isOK checkColEntry) es
      when (Seq.length es' < Seq.length es) $ do
        warn'Obj i $
          "cleanupImgRefs: col enties "
          <> toText (es, es')
          <> " removed in "
        adjustColEntries (const es') i

      -- recurse into subcollections
      foldColEntries go i md im be es


    checkRef :: (EffIStore r, EffError r)
             => ObjId -> SemMB r ImgNode
    checkRef i =
      getTreeAt i
      `bindMB`
      (\ n -> pureMB $ n ^. nodeVal)


    checkDirRef :: (EffIStore r, EffError r)
                => ObjId -> SemMB r ImgNode
    checkDirRef i =
      filterMB (\ n -> return $ isIMG n || isDIR n) (checkRef i)


    checkImgRef :: (EffIStore r, EffError r)
                => ObjId -> SemMB r ImgNode
    checkImgRef i =
      filterMB (\ n -> return $ isIMG n) (checkRef i)


    -- check whether both the ref and the part in an ImgRef exist
    checkImgPart :: (EffIStore r, EffError r)
                 => ImgRef -> SemMB r ImgNode
    checkImgPart (ImgRef i nm) =
      checkImgRef i
      `bindMB`
      (\ n -> (liftMB $ n ^? theParts . isoImgPartsMap . at nm)
              `bindMB`
              (\ _p -> pureMB n)
      )


    -- check a ColEntry ref for existence
    -- only the ImgRef's are checked, not the ColRef's
    checkColEntry :: (EffIStore r, EffError r)
                  => ColEntry -> SemMB r ColEntry
    checkColEntry ce =
      colEntry' imgRef colRef ce
      where
        colRef _ =
          pureMB ce
        imgRef ir =
          checkImgPart ir
          `bindMB`
          (\ _n -> pureMB ce)

    isOK :: (b -> SemMB r a) -> b -> Sem r Bool
    isOK cmd i = do
      mr <- cmd i
      return $ isJust mr

-- ----------------------------------------

checkImgStore :: SemISEJL r ()
checkImgStore = do
  log'verb "checkImgStore: check integrity of the archive"
  allCleanupImgRefs

  checkDeadObjIds
  checkUndefObjIds
  checkUsedImgObjIds
  checkUpLinkObjIds

  log'verb "checkImgStore: all checks done"

-- ----------------------------------------

checkUndefObjIds :: SemISEL r ()
checkUndefObjIds =
  getRootId >>= allUndefObjIds >>= showUndefObjIds
  where
    showUndefObjIds os
      | S.null os =
          log'trc "checkUndefObjIds: no undefined ObjIds found"
      | otherwise =
          log'warn $
          "checkUndefObjIds: inconsistent catalog\n"
          <> n ^. isoText
          <> " undefined ObjIds found\n"
          <> toText (toList os)
      where
        n = S.size os

-- ----------------------------------------

checkDeadObjIds :: SemISEJL r ()
checkDeadObjIds = do
  dos <- getRootId >>= allDeadObjIds
  showDeadObjIds dos
  cleanupDeadObjIds dos
  where

    allDeadObjIds :: ObjId -> SemISE r ObjIds
    allDeadObjIds t = do
      us <- allObjIds t
      as <- S.fromList . M.keys <$> gets (^. theImgTree . entries)
      return $ as `S.difference` us

    showDeadObjIds :: Member Logging r => ObjIds -> Sem r ()
    showDeadObjIds os
      | S.null os =
          log'trc "checkDeadObjIds: no dead ObjIds found"
      | otherwise =
          log'warn $
          "checkDeadObjIds: "
          <> n ^. isoText
          <> " undefined ObjIds found\n"
          <> toText (toList os)
      where
        n = S.size os

    cleanupDeadObjIds :: ObjIds -> SemISEJL r ()
    cleanupDeadObjIds ds =
      unless (S.null ds) $ do
        log'warn $ "checkDeadObjIds: removing dead ObjIds"
        modify' $ \ s -> s & theImgTree . entries %~ rmDeadRefs ds
          where
            rmDeadRefs os m = m `M.difference` M.fromSet (const ()) os

-- ----------------------------------------

checkUsedImgObjIds :: SemISEJL r ()
checkUsedImgObjIds = do
  ds <- getRootImgDirId >>= allImgObjIds
  cs <- getRootImgColId >>= allImgObjIds

  let orphanIds = ds `S.difference` cs
  let undefIds  = cs `S.difference` ds

  log'trc $
    "checkUsedImgObjIds: image  ids in dirs: " <> S.size ds ^. isoText
  log'trc $
    "checkUsedImgObjIds: image  ids in cols: " <> S.size cs ^. isoText
  log'trc $
    "checkUsedImgObjIds: orphan ids in dirs: " <> S.size orphanIds ^. isoText
  log'trc $
    "checkUsedImgObjIds: undef  ids in cols: " <> S.size undefIds ^. isoText

  showRes orphanIds undefIds
  cleanupOrphanIds orphanIds
  where

    showRes :: ObjIds -> ObjIds -> SemISEL r ()
    showRes ds cs = do
      case (nds, ncs) of
        (True,  True ) ->
          log'trc
          "checkUsedImgObjIds: image refs in collections and dir are the same"

        (False, True ) ->
          log'warn $
          "checkUsedImgObjIds: orphan image ids: "
          <> toText (toList ds)

        (True,  False) ->
          log'warn $
          "checkUsedImgObjIds: undefined image ids: "
          <> toText (toList cs)

        (False, False) -> do
          showRes mempty cs
          showRes ds mempty
      where
        nds = S.null ds
        ncs = S.null cs

    cleanupOrphanIds :: ObjIds -> SemISEJL r ()
    cleanupOrphanIds os
      | S.null os =
          return ()

      | otherwise = do
          log'trc $
            "checkUsedImgObjIds: removing "
            <> S.size os ^. isoText
            <> " orphan image ids "

          traverse_ rm os
            where
              rm i = do
                trc'Obj i "checkUsedImgObjIds: removing "
                rmImgNode i

-- ----------------------------------------

checkUpLinkObjIds :: SemISEJL r ()
checkUpLinkObjIds =
  getRootId >>= allWrongUpLinks >>= showWrongUpLinks
  where

    allWrongUpLinks :: ObjId -> SemISE r ObjIds
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

    showWrongUpLinks :: ObjIds -> SemISEL r ()
    showWrongUpLinks os
      | S.null os =
          log'trc "checkUpLinkObjIds: all uplinks o.k."

      | otherwise = do
          log'warn $
            "checkUpLinkObjIds: inconsistent catalog\n"
            <> S.size os ^. isoText
            <> " undefined ObjIds found\n"
            <> toText (toList os)

          foldObjIds showObj os
            where
              showObj i =
                getTreeAt i >>= maybe (return ()) msg
                where
                  msg n = do
                    warn'Obj i $
                      "checkUpLinkObjIds: node "
                      <> toText n
                      <> " contains element not pointing back to "

-- ----------------------------------------
