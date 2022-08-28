module Catalog.ImgTree.Check
  ( checkInvImgTree
  , statsImgTree
  )
where

import Data.Prim
import Data.ImgTree
import Data.ImageStore

import Catalog.Effects
import Catalog.ImgTree.Modify
import Catalog.ImgTree.Access

-- -----------------------------------------

cleanupAllDirJunk :: Eff'ISEJL r => Fold NavTree (ObjId, ObjId)
                     -> Sem r ()
cleanupAllDirJunk check = do
  log'warn "cleanupAllDirJunk: remove illegal ref in DIR entries"
  t <- mkNavTree <$> dt

  let cmds = t ^.. check . to (uncurry removeRef)

  log'trc $ "cleanupAllDirJunk: # of corrected refs: " <> length cmds ^. isoText
  sequenceA_ cmds

cleanupColRefJunk :: Eff'ISEJL r => Fold NavTree (ObjId, ObjId)
                     -> Sem r ()
cleanupColRefJunk check = do
  log'warn "cleanupColRefJunk: remove illegal ref in COL entries"
  t <- mkNavTree <$> dt

  let cmds = t ^.. check . to (uncurry removeRef)

  log'trc $ "cleanupColRefJunk: # of corrected refs: " <> length cmds ^. isoText
  sequenceA_ cmds

cleanupImgNameJunk :: Eff'ISEJL r => Fold NavTree (ImgRef, ObjId)
                   -> Sem r ()
cleanupImgNameJunk check = do
  log'warn "cleanupImgNameJunk: remove img ref with illegal names in COL entries"
  t <- mkNavTree <$> dt

  let cmds = t ^.. check . to (uncurry removeImgRef)

  log'trc $ "cleanupImgNameJunk: # of corrected refs: " <> length cmds ^. isoText
  sequenceA_ cmds

-- --------------------

repairAllUplinks :: Eff'ISEJL r => Fold NavTree ((ObjId, ObjId), ObjId)
                 -> Sem r ()
repairAllUplinks check = do
  log'warn "repairAllUplinks: remove or repair ref in parent entry"
  t <- mkNavTree <$> dt

  let edit ((r, pr), wpr)
        | wpr'contains'r  = removeRef    r pr
        | otherwise       = repairUplink r pr
        where
         wpr'contains'r =
           has (setCur t . theNode . imgNodeRefs . filtered (== r)) wpr

  let cmds = t ^.. check . to edit

  log'trc $ "repairAllUplinks: # of corrected refs: " <> length cmds ^. isoText
  sequenceA_ cmds

-- --------------------

removeAllOrphans :: Eff'ISEJL r => Fold NavTree ObjId
                 -> Sem r ()
removeAllOrphans check = do
  log'warn "removeAllOrphans: remove refs from map"
  t <- mkNavTree <$> dt

  let cmds = t ^.. check
                 . to removeOrphan

  log'trc $ "removeAllOrphans # of orphan refs: " <> length cmds ^. isoText
  sequenceA_ cmds

-- --------------------
-- cleanup helper

clearColBlog :: Eff'ISEJL r => ObjId -> Sem r ()
clearColBlog ref = do
  log'ref "clear COL blog ref" ref
  adjustColBlog (const Nothing) ref

clearColImg :: Eff'ISEJL r => ObjId -> Sem r ()
clearColImg ref = do
  log'ref "clear COL img ref" ref
  adjustColImg (const Nothing) ref

repairUplink :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
repairUplink ref pref = do
  log'ref ("repair parent ref to " <> fmtR pref) ref
  modify' (\ s -> s & theImgTree . entries . emAt ref . parentRef .~ pref)

removeOrphan :: Eff'ISEJL r => ObjId -> Sem r ()
removeOrphan ref = do
  log'ref ("remove orphan ref") ref
  modify' (\ s -> s & theImgTree . entries . emAt ref .~ mempty)

removeRef :: Eff'ISEJL r => ObjId -> ObjId -> Sem r ()
removeRef refrem ref = do
  log'trc $ indent "ref: " <> fmtR refrem
    <> " removed from entry with ref: " <> fmtR ref

  ((, ref) <$> dt) >>= edit
  where
    edit t
      | isDIR  n  = adjustDirEntries
                    ( const $
                      filterDirEntries  (/= refrem)
                                        (n ^. theDirEntries)
                    )
                    ref
      | isCOL  n  = sequenceA_ [adjImg, adjBlog, adjEntries]

      | otherwise = return ()
      where
        n = t ^. theNode

        adjImg
          | has (theColImg . traverse . imgref . filtered (== refrem)) n =
              clearColImg ref
          | otherwise =
              return ()

        adjBlog
          | has (theColBlog . traverse . imgref . filtered (== refrem)) n =
              clearColBlog ref
          | otherwise =
              return ()

        adjEntries =
          adjustColEntries
          ( const $
            filterColEntries (colEntry'
                               ((/= refrem) . (^. imgref))
                                (/= refrem)
                             )
                             ( n ^. theColEntries)
          )
          ref

removeImgRef :: Eff'ISEJL r => ImgRef -> ObjId -> Sem r ()
removeImgRef ir ref = do
  log'trc $ indent "img ref: " <> fmtIR ir
    <> " removed from entry with ref: " <> fmtR ref

  ((, ref) <$> dt) >>= edit
  where
    edit t
      | isCOL  n  = sequenceA_ [adjImg, adjBlog, adjEntries]

      | otherwise = return ()
      where
        n = t ^. theNode

        adjImg
          | has (theColImg . traverse . filtered (== ir)) n =
              clearColImg ref
          | otherwise =
              return ()

        adjBlog
          | has (theColBlog . traverse . filtered (== ir)) n =
              clearColBlog ref
          | otherwise =
              return ()

        adjEntries =
          adjustColEntries
          ( const $
            filterColEntries (colEntry' (/= ir) (const True))
                             ( n ^. theColEntries)
          )
          ref

log'ref :: Eff'ISEL r => Text -> ObjId -> Sem r ()
log'ref txt ref =
  log'trc $ indent "ref: " <> fmtR ref <> ": " <> txt

-- --------------------

checkInvImgTree :: Eff'ISEJL r => Sem r ()
checkInvImgTree = do
  log'verb "ImgTree invariant check"

  -- root node
  asRootNode
  asRootColNode
  asRootDirNode
  asRootChildRef

  -- check all parent refs point to the parent entry
  asUplink

  -- refs point to entries of the correct type
  -- all img names in img refs exist
  asNoJunkInDir
  asNoJunkInColRefs
  asNoJunkInPartNames

  -- no junk in map of entries
  -- cleanup in previous steps
  -- my lead to orphan entries
  asNoOrphans

  log'verb "ImgTree invariant check and cleanup done"
  return ()

  where
    asRootNode    = check'
                    "check root ref"
                    "root ref doesn't point to a ROOT node"
                    fmtNode'
                    (const $ abortWith "archive corrupted")
                    (theNode . filtered (not . isROOT))

    asRootColNode = check'
                    "check root collection ref"
                    "root collection ref doesn't point to a COL node"
                    fmtNode'
                    (const $ abortWith "archive corrupted")
                    (theRootCol . theNode . filtered (not . isCOL))

    asRootDirNode = check'
                    "check root directory ref"
                    "root dir ref doesn't point to a DIR node"
                    fmtNode'
                    (const $ abortWith "archive corrupted")
                    (theRootDir . theNode . filtered (not . isDIR))

    asRootChildRef = check'
                     "check parent ref of root children"
                     "parent ref in col or dir entry wrong"
                     fmtRef2
                     (const $ abortWith "archive corrupted")
                     ( folding
                       (\ t@(_, rr) ->
                          t ^.. theChildren
                              . theEntry
                              . parentRef
                              . filtered (/= rr)
                              . to (,rr)
                       )
                     )

    asNoJunkInDir  = check'
                     "check references in DIR nodes"
                     "none IMG or none DIR node(s) found in dir hierachy"
                     fmtRef2d
                     cleanupAllDirJunk
                     noJunkInDirs

    asNoJunkInColRefs = check'
                     "check references in COL nodes"
                     "none IMG or none COL node(s) found in col hierachy"
                     fmtRef2d
                     cleanupColRefJunk
                     noJunkInColRefs

    asNoJunkInPartNames = check'
                     "check referenced part names in COL nodes"
                     "illegal part names found in col entries"
                     fmtRef2c
                     cleanupImgNameJunk
                     noJunkInPartNames

    asUplink       = check'
                     "check parent refs"
                     "parent ref(s) don't point to parent entries"
                     fmtRef3
                     repairAllUplinks
                     uplinkCheck

    asNoOrphans    = check'
                     "check orphan refs"
                     "orphans ref(s) found"
                     fmtObjId
                     removeAllOrphans
                     allOrphanObjIds

    -- exec a single check and cleanup

    check' :: Eff'ISEJL r
           => Text                         -- title of check
           -> Text                         -- error message
           -> (a -> Text)                  -- format result data
           -> (Fold NavTree a -> Sem r ()) -- repair action
           -> Fold NavTree a               -- check result
           -> Sem r ()

    check' tt msg fmt cleanup query = do
      t <- mkNavTree <$> dt
      if has query t
        then do
          log'warn l1
          log'warn l2
          traverseOf_ query (log'warn . fmt) t
          cleanup query
        else do
          log'trc  (l1 <> ": O.K.")
      where
        l1 = "checkInvImgTree: " <> tt
        l2 = "checkInvImgTree: " <> msg

    fmtNode' :: ImgNode -> Text
    fmtNode' n = n ^. to show . isoText . to (indent . ("node: " <>))

    fmtObjId :: ObjId -> Text
    fmtObjId = indent . ("ref: " <>) . fmtR

    fmtRef2 :: (ObjId, ObjId) -> Text
    fmtRef2 (wrr, rr) =
      indent "expected: " <> fmtR rr
      <>
      ", found: " <> fmtR wrr

    fmtRef2c :: (ImgRef, ObjId) -> Text
    fmtRef2c (wir, rr) =
      indent "col ref: " <> fmtR rr
      <>
      ", illegal img part name in img ref: "
      <> fmtIR wir

    fmtRef2d :: (ObjId, ObjId) -> Text
    fmtRef2d (wrr, rr) =
      indent "in entry: " <> fmtR rr
      <>
      ", illegal ref found: " <> fmtR wrr

    fmtRef3 :: ((ObjId, ObjId), ObjId) -> Text
    fmtRef3 ((r, pr), wpr) =
      indent "ref: " <> fmtR r
      <>
      ": wrong parent ref: " <> fmtR wpr
      <>
      ", should be ref: " <> fmtR pr

fmtIR :: ImgRef -> Text
fmtIR ir =
  "("  <> fmtR (ir ^. imgref) <>
  ", " <> (ir ^. imgname ^. isoText) <>
  ")"

fmtR :: ObjId -> Text
fmtR r = show r ^. isoText

indent :: Text -> Text
indent = ("  " <>)

------------------------------------------------------------------------------

type NodeStats = (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)

nodeStats :: Getter ImgNode NodeStats
nodeStats = to f
  where
    f ROOT{} = (Sum 1, mempty, mempty, mempty, mempty)
    f COL{}  = (mempty, Sum 1, mempty, mempty, mempty)
    f DIR{}  = (mempty, mempty, Sum 1, mempty, mempty)
    f IMG{}  = (mempty, mempty, mempty, Sum 1, mempty)
    f NONO   = (mempty, mempty, mempty, mempty, Sum 1)


imgTreeStats :: Fold NavTree NodeStats
imgTreeStats = allEntries . theNode . nodeStats

statsImgTree :: Eff'ISEL r => Sem r ()
statsImgTree = do
  t <- mkNavTree <$> dt

  let (nr, nc, nd, ni, nu) = t ^. imgTreeStats
  logStat "ROOT" nr
  logStat "COL " nc
  logStat "DIR " nd
  logStat "IMG " ni
  logStat "NULL" nu
  return ()

  where
    logStat t (Sum n) =
      log'info $ "# of " <> t <> " node refs: " <> n ^. isoText

------------------------------------------------------------------------------
