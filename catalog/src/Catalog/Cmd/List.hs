module Catalog.Cmd.List
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.ImageStore
import           Data.ImageTree
import           Data.Prim
import           Data.RefTree

-- ----------------------------------------

listNames :: ObjId -> Cmd String
listNames i0 =
  unlines <$> foldMT imgA dirA rootA colA i0
  where
    nm i     = show <$> getImgName i
    ind n xs = n : map ("  " ++) xs

    imgA i ps = do
      n <- nm i
      return $
        ind n (ps ^.. isoImgParts . traverse . theImgName . name2string)

    dirA go i es _ts = do
      n  <- nm i
      xs <- mapM go (es ^. isoSetList)
      return $
        ind n (concat xs)

    rootA go i dir col = do
      n   <- nm i
      dns <- go dir
      cns <- go col
      return $
        ind n (dns ++ cns)

    colA go i _md es _ts = do
      n   <- nm i
      cns <- mapM go' es
      return $
        ind n (concat cns)
      where
        go' (ImgRef _i n) =
          return [n ^. name2string]
        go' (ColRef i') =
          go i'

listPaths' :: ObjId -> Cmd [Path]
listPaths' =
  foldMT imgA dirA rootA colA
  where
    imgA i ps = do
      p  <- objid2path i
      let pp = ps ^.. isoImgParts . traverse . theImgName . to (`substPathName` p)
      return $
        p : pp

    dirA go i es _ts = do
      p  <- objid2path i
      pp <- mapM go (es ^. isoSetList)
      return $
        p : concat pp

    rootA go i dir col = do
      p  <- objid2path i
      pd <- go dir
      pc <- go col
      return $
        p : pd ++ pc

    colA go i _md es _ts = do
      p  <- objid2path i
      pp <- mapM go' es
      return $
        p : concat pp
      where
        go' :: ColEntry -> Cmd [Path]
        go' (ImgRef i' n') = do
          ip <- objid2path i'
          return [substPathName n' ip]
        go' (ColRef i') =
          go i'

listPaths :: ObjId -> Cmd String
listPaths i = (unlines . map show) <$> listPaths' i

listImages' :: Cmd [(Path, [Name])]
listImages' = do
  r <- use (theImgTree . rootRef)
  processImages listImg r
  where
    listImg :: ObjId -> ImgParts -> Cmd [(Path, [Name])]
    listImg i ps = do
      p <- objid2path i
      let pns = ps ^.. isoImgParts . traverse . theImgName
      return [(p, pns)]

listImages :: Cmd String
listImages = formatImages <$> listImages'
  where
    formatImages :: [(Path, [Name])] -> String
    formatImages = unlines . map (uncurry fmt)
      where
        fmt p ns = show p ++ ": " ++ intercalate ", " (map show ns)

-- ----------------------------------------