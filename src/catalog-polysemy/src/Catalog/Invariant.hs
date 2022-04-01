------------------------------------------------------------------------------

module Catalog.Invariant
  ( checkImgStore )
where

import Catalog.Effects
       ( Sem
       , EffError
       , EffIStore
       , Eff'ISE
       , Eff'ISEL
       , Eff'ISEJL
       , Logging
       , Member
       , NonDet
       , Sem
       , gets
       , liftMaybe
       , log'trc
       , log'verb
       , log'warn
       , modify'
       , pureMaybe
       , runMaybe
       )
import Catalog.ImgTree.Fold
       ( foldDir
       , allImgObjIds
       , allObjIds
       , allUndefObjIds
       , foldColEntries
       , foldRoot
       , ignoreImg
       , foldMTU
       )
import Catalog.ImgTree.Access
       ( foldObjIds
       , getImgParent
       , bitsUsedInImgTreeMap
       , getRootImgColId
       , getRootImgDirId
       , getTreeAt
       , getRootId
       )
import Catalog.ImgTree.Modify
       ( rmImgNode
       , adjustColEntries
       , adjustColBlog
       , adjustColImg
       , adjustDirEntries
       )
import Catalog.Logging
       ( trc'Obj
       , warn'Obj
       , verb'Obj
       )

import Data.ImageStore
       ( theImgTree )

import Data.ImgTree
       ( ColEntry
       , ImgNode
       , ImgRef
       , ImgRef'(ImgRef)
       , ObjIds
       , colEntry'
       , entries
       , isDIR
       , isIMG
       , isoDirEntries
       , isoImgPartsMap
       , nodeVal
       , theColColRef
       , theParts
       )
import Data.Prim
       ( Alternative(empty)
       , At(at)
       , Foldable(toList)
       , IsoText(isoText)
       , ObjId
       , (#)
       , (%~)
       , (&)
       , (^.)
       , (^..)
       , (^?)
       , filterSeqM
       , isJust
       , toText
       , traverse_
       , unless
       , when
       )

import qualified Data.Set        as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as Seq

-- ----------------------------------------

allCleanupImgRefs :: Eff'ISEJL r => Sem r ()
allCleanupImgRefs = getRootId >>= cleanupImgRefs

cleanupImgRefs :: Eff'ISEJL r => ObjId -> Sem r ()
cleanupImgRefs i0 = do
  verb'Obj i0 "cleanupImgRefs: remove outdated img refs in "

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

      b1 <- maybe (return True) (isOK checkImgPart) im

      unless b1 $ do
        warn'Obj i $
          "cleanupImgRefs: col img ref "
          <> toText im
          <> " removed in "
        adjustColImg (const Nothing) i

      b2 <- maybe (return True) (isOK checkImgPart) be

      unless b2 $ do
        warn'Obj i $
          "cleanupImgRefs: col blog ref"
          <> toText be
          <> " removed in "
        adjustColBlog (const Nothing) i

      es' <- filterSeqM (isOK checkColEntry) es
      when (Seq.length es' < Seq.length es) $ do
        warn'Obj i $
          "cleanupImgRefs: col enties "
          <> toText (es, es')
          <> " removed in "
        adjustColEntries (const es') i

      -- recurse into subcollections
      foldColEntries go i md im be es

----------------------------------------

    checkRef :: (EffIStore r, EffError r, Member NonDet r)
             => ObjId -> Sem r ImgNode
    checkRef i = do
      (^. nodeVal) <$> liftMaybe (getTreeAt i)

    checkDirRef :: (EffIStore r, EffError r, Member NonDet r)
                => ObjId -> Sem r ImgNode
    checkDirRef i =
      filterM' (\ n -> return $ isIMG n || isDIR n) (checkRef i)

    checkImgRef :: (EffIStore r, EffError r, Member NonDet r)
                => ObjId -> Sem r ImgNode
    checkImgRef i =
      filterM' (return . isIMG) (checkRef i)

    -- check whether both the ref and the part in an ImgRef exist
    checkImgPart :: (EffIStore r, EffError r, Member NonDet r)
                 => ImgRef -> Sem r ImgNode
    checkImgPart (ImgRef i nm) = do
      n1 <- checkImgRef i
      _r <- pureMaybe (n1 ^? theParts . isoImgPartsMap . at nm)
      return n1

    -- check a ColEntry ref for existence
    -- only the ImgRef's are checked, not the ColRef's
    checkColEntry :: (EffIStore r, EffError r, Member NonDet r)
                   => ColEntry -> Sem r ColEntry
    checkColEntry ce =
      colEntry' imgRef colRef ce
      where
        colRef _ =
          return ce
        imgRef ir = do
          _n <- checkImgPart ir
          return ce

    isOK :: (b -> Sem (NonDet ': r) a) -> b -> Sem r Bool
    isOK cmd i =
      isJust <$> runMaybe (cmd i)

    filterM' :: (EffIStore r, EffError r, Member NonDet r)
             => (a -> Sem r Bool) -> Sem r a -> Sem r a
    filterM' pr cmd = do
      x <- cmd
      b <- pr x
      if b
        then return x
        else empty

-- ----------------------------------------

checkImgStore :: Eff'ISEJL r => Sem r ()
checkImgStore = do
  log'verb "checkImgStore: check integrity of the archive"
  allCleanupImgRefs

  checkDeadObjIds
  checkUndefObjIds
  checkUsedImgObjIds
  checkUpLinkObjIds

  log'verb "checkImgStore: all checks done"

-- ----------------------------------------

checkUndefObjIds :: Eff'ISEL r => Sem r ()
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

checkDeadObjIds :: Eff'ISEJL r => Sem r ()
checkDeadObjIds = do
  dos <- getRootId >>= allDeadObjIds
  showDeadObjIds dos
  cleanupDeadObjIds dos
  where

    allDeadObjIds :: Eff'ISE r => ObjId -> Sem r ObjIds
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

    cleanupDeadObjIds :: Eff'ISEJL r => ObjIds -> Sem r ()
    cleanupDeadObjIds ds =
      unless (S.null ds) $ do
        log'warn "checkDeadObjIds: removing dead ObjIds"
        modify' $ \ s -> s & theImgTree . entries %~ rmDeadRefs ds
          where
            rmDeadRefs os m = m `M.difference` M.fromSet (const ()) os

-- ----------------------------------------

checkUsedImgObjIds :: Eff'ISEJL r => Sem r ()
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

  bits <- bitsUsedInImgTreeMap
  log'trc $
    "checkUsedImgObjids: # bits used in ImgTree ObjIds: " <> toText bits

  showRes orphanIds undefIds
  cleanupOrphanIds orphanIds
  where

    showRes :: Eff'ISEL r => ObjIds -> ObjIds -> Sem r ()
    showRes ds cs = do
      case (nds, ncs) of
        (True,  True ) ->
          log'trc
          "checkUsedImgObjIds: ok, image refs in collections and dir are the same"

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

    cleanupOrphanIds :: Eff'ISEJL r => ObjIds -> Sem r ()
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

checkUpLinkObjIds :: Eff'ISEJL r => Sem r ()
checkUpLinkObjIds =
  getRootId >>= allWrongUpLinks >>= showWrongUpLinks
  where

    allWrongUpLinks :: Eff'ISE r => ObjId -> Sem r ObjIds
    allWrongUpLinks =
      foldMTU ignoreImg dirA rootA colA
      where
        dirA go i es ts = do
          s0 <- toObjIds i
                <$>
                traverse getImgParent es
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

    showWrongUpLinks :: Eff'ISEL r => ObjIds -> Sem r ()
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

------------------------------------------------------------------------
