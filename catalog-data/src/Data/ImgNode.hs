{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.ImgNode
       ( ImgNode'(..)
       , ImgParts
       , ImgPart
       , ImgRef'(..)
       , ImgRef
       , ColEntry'(..)
       , ColEntries'
       , ColEntrySet'
       , ColEntrySet
       , DirEntries'
       , mkImgParts
       , mkImgPart
       , mkColImgRef
       , mkColImgRef'
       , mkColColRef
       , mkDirEntries
       , emptyImg
       , emptyImgDir
       , emptyImgRef
       , emptyImgRoot
       , emptyImgCol
       , isDIR
       , isIMG
       , isROOT
       , isCOL
       , isColColRef
       , isColImgRef
       , colEntry
       , colEntry'
       , isoImgParts
       , isoImgPartsMap
       , isoDirEntries
       , theParts
       , thePartNames'
       , thePartNames
       , thePartNamesI
       , theImgName
       , theImgPart
       , theImgType
       , theImgTimeStamp
       , theImgCheckSum
       , theDir
       , theDirEntries
       , theMetaData
       , theSyncTime
       , theRootImgDir
       , theRootImgCol
       , theImgRoot
       , theImgCol
       , theColObjId
       , theColMetaData
       , theColImg
       , theColBlog
       , theColEntries
       , theColColRef
       , theColImgRef
       , addDirEntry
       , delDirEntry
       , delColEntry
       , memberColEntrySet
       , singletonColEntrySet
       , fromListColEntrySet
       , fromSeqColEntrySet
       , toListColEntrySet
       , toSeqColEntrySet
       , diffColEntrySet
       , intersectColEntrySet
       , ObjIds
       , singleObjId
       , isWriteableCol
       , isSortableCol
       , isRemovableCol
       , ColRef'
       , ColRef
       , cColRef
       )
where

import           Control.Monad.Except
import           Data.MetaData
import           Data.Prim

import qualified Data.Aeson      as J
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Sequence   as Seq

-- ----------------------------------------


data ImgNode' ref = IMG  !ImgParts
                         !MetaData               -- image meta data other
                                                 -- than exif data
                  | DIR  !(DirEntries' ref)      -- the contents of an image dir
                         !TimeStamp              -- the last sync with
                                                 -- the file system
                  | ROOT !ref !ref
                  | COL  !MetaData               -- collection meta data
                         !(Maybe (ImgRef' ref))  -- optional image
                         !(Maybe (ImgRef' ref))  -- optional blog entry
                         !(ColEntries'    ref)   -- the list of images
                                                 -- and subcollections

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

deriving instance Functor ImgNode'

instance IsEmpty (ImgNode' ref) where
  isempty (IMG _pts _md)       = True
  isempty (DIR es _ts)         = isempty es
  isempty (COL _md _im _be cs) = isempty cs
  isempty (ROOT _d _c)         = False

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm md) = J.object
    [ "ImgNode"     J..= ("IMG" :: String)
    , "parts"       J..= pm
    , "metadata"    J..= md
    ]
  toJSON (DIR rs ts) = J.object
    [ "ImgNode"     J..= ("DIR" :: String)
    , "children"    J..= rs
    , "sync"        J..= ts
    ]
  toJSON (ROOT rd rc) = J.object
    [ "ImgNode"     J..= ("ROOT" :: String)
    , t'archive     J..= rd
    , t'collections J..= rc
    ]
  toJSON (COL md im be es) = J.object $
    [ "ImgNode"    J..= ("COL" :: String)
    , "metadata"   J..= md
    , "entries"    J..= es
    ]
    ++ case im of
         Nothing -> []
         Just (ImgRef i n)  -> ["image" J..= (i, n)]
    ++ case be of
         Nothing -> []
         Just (ImgRef i n)  -> ["blog"  J..= (i, n)]

instance (FromJSON ref) => FromJSON (ImgNode' ref) where
  parseJSON = J.withObject "ImgNode" $ \ o ->
    do t <- o J..: "ImgNode"
       case t :: String of
         "IMG" ->
           IMG  <$> o J..: "parts"
                <*> o J..: "metadata"
         "DIR" ->
           DIR  <$> o J..: "children"
                <*> o J..:? "sync" J..!= mempty
         "ROOT" ->
           ROOT <$> o J..: t'archive
                <*> o J..: t'collections
         "COL" ->
           COL  <$> o J..: "metadata"
                <*> ((uncurry ImgRef <$>) <$> o J..:? "image" J..!= Nothing)
                <*> ((uncurry ImgRef <$>) <$> o J..:? "blog"  J..!= Nothing)
                <*> o J..: "entries"
         _ -> mzero

emptyImgDir :: ImgNode' ref
emptyImgDir = DIR mempty mempty
{-# INLINE emptyImgDir #-}

emptyImg :: ImgNode' ref
emptyImg = IMG mempty mempty
{-# INLINE emptyImg #-}

emptyImgRoot :: Monoid ref => ImgNode' ref
emptyImgRoot = ROOT mempty mempty
{-# INLINE emptyImgRoot #-}

emptyImgCol :: ImgNode' ref
emptyImgCol = COL mempty Nothing Nothing mempty
{-# INLINE emptyImgCol #-}

-- image node optics

thePartsMd :: Prism' (ImgNode' ref) (ImgParts, MetaData)
thePartsMd
  = prism (uncurry IMG)
          (\ x -> case x of
                  IMG pm md -> Right (pm, md)
                  _         -> Left  x
              )
{-# INLINE thePartsMd #-}

theParts :: Traversal' (ImgNode' ref) ImgParts
theParts = thePartsMd . _1
{-# INLINE theParts #-}

theImgPart :: Name -> Traversal' (ImgNode' ref) ImgPart
theImgPart nm = theParts . isoImgPartsMap . ix nm
{-# INLINE theImgPart #-}

theDir :: Prism' (ImgNode' ref) (DirEntries' ref, TimeStamp)
theDir =
  prism (uncurry DIR)
        (\ x -> case x of
                DIR s t -> Right (s, t)
                _       -> Left  x
          )
{-# INLINE theDir #-}

theDirEntries :: Traversal' (ImgNode' ref) (DirEntries' ref)
theDirEntries = theDir . _1
{-# INLINE theDirEntries #-}

-- traverseWords :: Traversal' State Word8
-- traverseWords :: Applicative f => (Word8 -> f Word8) -> State -> f State
-- traverseWords inj (State wa wb) = State <$> inj wa <*> inj wb

theMetaData :: Traversal' (ImgNode' ref) MetaData
theMetaData inj (IMG pm md)
  = IMG pm <$> inj md
theMetaData inj (COL md im be es)
  = COL <$> inj md <*> pure im <*> pure be <*> pure es
theMetaData _   n
  = pure n
{-# INLINE theMetaData #-}

theSyncTime :: Traversal' (ImgNode' ref) TimeStamp
theSyncTime inj (DIR es ts)          = DIR es <$> inj ts
theSyncTime _   n                    = pure n
{-# INLINE theSyncTime #-}

theImgRoot :: Prism' (ImgNode' ref) (ref, ref)
theImgRoot =
  prism (uncurry ROOT)
        (\ x -> case x of
            ROOT rd rc -> Right (rd, rc)
            _          -> Left x
        )
{-# INLINE theImgRoot #-}

theRootImgDir :: Traversal' (ImgNode' ref) ref
theRootImgDir = theImgRoot . _1
{-# INLINE theRootImgDir #-}

theRootImgCol :: Traversal' (ImgNode' ref) ref
theRootImgCol = theImgRoot . _2
{-# INLINE theRootImgCol #-}

theImgCol :: Prism' (ImgNode' ref)
                    (MetaData, Maybe (ImgRef' ref), Maybe (ImgRef' ref), (ColEntries' ref))
theImgCol =
  prism (\ (x1, x2, x3, x4) -> COL x1 x2 x3 x4)
        (\ x -> case x of
            COL x1 x2 x3 x4 -> Right (x1, x2, x3, x4)
            _               -> Left x
        )
{-# INLINE theImgCol #-}

theColMetaData :: Traversal' (ImgNode' ref) MetaData
theColMetaData = theImgCol . _1
{-# INLINE theColMetaData #-}

theColImg :: Traversal' (ImgNode' ref) (Maybe (ImgRef' ref))
theColImg = theImgCol . _2
{-# INLINE theColImg #-}

theColBlog :: Traversal' (ImgNode' ref) (Maybe (ImgRef' ref))
theColBlog = theImgCol . _3
{-# INLINE theColBlog #-}

theColEntries :: Traversal' (ImgNode' ref) (ColEntries' ref)
theColEntries = theImgCol . _4
{-# INLINE theColEntries #-}

isDIR :: ImgNode' ref -> Bool
isDIR DIR{}  = True
isDIR _      = False
{-# INLINE isDIR #-}

isIMG :: ImgNode' ref -> Bool
isIMG IMG{}  = True
isIMG _      = False
{-# INLINE isIMG #-}

isROOT :: ImgNode' ref -> Bool
isROOT ROOT{} = True
isROOT _      = False
{-# INLINE isROOT #-}

isCOL :: ImgNode' ref -> Bool
isCOL COL{} = True
isCOL _     = False
{-# INLINE isCOL #-}

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance IsEmpty ImgParts where
  isempty (ImgParts im) = isempty im
  {-# INLINE isempty #-}

instance Semigroup ImgParts where
  ImgParts m1 <> ImgParts m2
    = ImgParts $ M.mergeWithKey combine only1 only2 m1 m2
    where
      only1 = const M.empty
      only2 = id
      combine _k e1 e2
        | t1 >= t2  = Just e1
        | otherwise = Just e2
        where
          t1 = e1 ^. theImgTimeStamp
          t2 = e2 ^. theImgTimeStamp

instance Monoid ImgParts where
  mempty  = ImgParts M.empty
  mappend = (<>)

instance ToJSON ImgParts where
  toJSON (ImgParts pm) = toJSON . M.toList $ pm
  {-# INLINE toJSON #-}

instance FromJSON ImgParts where
  parseJSON x = (ImgParts . M.fromList) <$> parseJSON x

mkImgParts :: [ImgPart] -> ImgParts
mkImgParts ps = isoImgParts # ps
{-# INLINE mkImgParts #-}

isoImgParts :: Iso' ImgParts [ImgPart]
isoImgParts =
  iso (\ (ImgParts pm) -> pm) ImgParts
  .
  isoMapElems (\ (IP n _ _ _) -> n)
{-# INLINE isoImgParts #-}

isoImgPartsMap :: Iso' ImgParts (Map Name ImgPart)
isoImgPartsMap = iso (\ (ImgParts pm) -> pm) ImgParts

thePartNames' :: (ImgType -> Bool) -> Traversal' ImgParts Name
thePartNames' typTest =
  isoImgParts . traverse . isA (^. theImgType . to typTest) . theImgName
{-# INLINE thePartNames' #-}

-- images with 1 of the given types can be rendered
thePartNamesI :: Traversal' ImgParts Name
thePartNamesI = thePartNames' isShowablePart
{-# INLINE thePartNamesI #-}

thePartNames :: Traversal' ImgParts Name
thePartNames = thePartNames' (const True)
{-# INLINE thePartNames #-}

-- ----------------------------------------

data ImgPart     = IP !Name !ImgType !TimeStamp !CheckSum

deriving instance Show ImgPart

instance ToJSON ImgPart where
  toJSON (IP n t s c) = J.object $
    [ "Name"      J..= n
    , "ImgType"   J..= t
    ]
    ++ ("TimeStamp" .=?! s)       -- optional field
    ++ ("CheckSum"  .=?! c)       --     "      "

instance FromJSON ImgPart where
  parseJSON = J.withObject "ImgPart" $ \ o ->
    IP <$> o J..:   "Name"
       <*> o J..:   "ImgType"
       <*> o   .:?! "TimeStamp"   -- optional field
       <*> o   .:?! "CheckSum"    --    "       "

mkImgPart :: Name -> ImgType -> ImgPart
mkImgPart n t = IP n t mempty mempty
{-# INLINE mkImgPart #-}

theImgName :: Lens' ImgPart Name
theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n
{-# INLINE theImgName #-}

theImgType :: Lens' ImgPart ImgType
theImgType k (IP n t s c) = (\ new -> IP n new s c) <$> k t
{-# INLINE theImgType #-}

theImgTimeStamp :: Lens' ImgPart TimeStamp
theImgTimeStamp k (IP n t s c) = (\ new -> IP n t new c) <$> k s
{-# INLINE theImgTimeStamp #-}

theImgCheckSum :: Lens' ImgPart CheckSum
theImgCheckSum k (IP n t s c) = (\ new -> IP n t s new) <$> k c
{-# INLINE theImgCheckSum #-}

-- ----------------------------------------

data ImgRef' ref = ImgRef {_iref :: !ref, _iname ::  !Name}
type ImgRef      = ImgRef' ObjId

deriving instance (Eq   ref) => Eq   (ImgRef' ref)
deriving instance (Ord  ref) => Ord  (ImgRef' ref)
deriving instance (Show ref) => Show (ImgRef' ref)
deriving instance Functor     ImgRef'
deriving instance Foldable    ImgRef'
deriving instance Traversable ImgRef'

instance IsEmpty (ImgRef' ref) where
  isempty = isempty . _iname

emptyImgRef :: ImgRef
emptyImgRef = ImgRef mempty mempty

-- --------------------

data ColEntry'   ref  = ImgEnt ! (ImgRef' ref)
                      | ColEnt ! ref

type ColEntries' ref = Seq (ColEntry' ref)

deriving instance (Eq   ref) => Eq   (ColEntry' ref)
deriving instance (Ord  ref) => Ord  (ColEntry' ref)
deriving instance (Show ref) => Show (ColEntry' ref)
deriving instance Functor     ColEntry'
deriving instance Foldable    ColEntry'
deriving instance Traversable ColEntry'

instance (ToJSON ref) => ToJSON (ColEntry' ref) where
  toJSON (ImgEnt (ImgRef i n)) = J.object $
    [ "ColEntry"  J..= ("IMG" :: String)
    , "ref"       J..= i
    , "part"      J..= n
    ]

  toJSON (ColEnt i) = J.object
    [ "ColEntry"  J..= ("COL" :: String)
    , "ref"       J..= i
    ]

instance (FromJSON ref) => FromJSON (ColEntry' ref) where
  parseJSON = J.withObject "ColEntry" $ \ o ->
    do t <- o J..: "ColEntry"
       case t :: String of
         "IMG" ->
           ImgEnt <$> (ImgRef <$> o J..: "ref"
                              <*> o J..: "part"
                      )
         "COL" ->
           ColEnt <$> o J..: "ref"
         _ -> mzero

mkColImgRef :: ref -> Name -> ColEntry' ref
mkColImgRef i n = ImgEnt $ ImgRef i n
{-# INLINE mkColImgRef #-}

mkColImgRef' :: ImgRef' ref -> ColEntry' ref
mkColImgRef' = ImgEnt
{-# INLINE mkColImgRef' #-}

mkColColRef :: ref -> ColEntry' ref
mkColColRef = ColEnt
{-# INLINE mkColColRef #-}

colEntry :: (ref -> Name -> a) ->
            (ref         -> a) ->
            ColEntry' ref -> a
colEntry  imgRef _colRef (ImgEnt (ImgRef i n)) = imgRef i n
colEntry _imgRef  colRef (ColEnt i           ) = colRef i

colEntry' :: (ImgRef' ref -> a) ->
             (ref         -> a) ->
             ColEntry' ref -> a
colEntry'  imgRef _colRef (ImgEnt ir) = imgRef ir
colEntry' _imgRef  colRef (ColEnt i ) = colRef i


theColObjId :: Lens' (ColEntry' ref) ref
theColObjId k (ImgEnt (ImgRef i n)) = (\ new -> ImgEnt (ImgRef new n)) <$> k i
theColObjId k (ColEnt i           ) =           ColEnt                 <$> k i
{-# INLINE theColObjId #-}

-- theImgName :: Lens' ImgPart Name
-- theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n

theColImgRef :: Prism' (ColEntry' ref) (ref, Name)
theColImgRef =
  prism (\ (i, n) -> ImgEnt (ImgRef i n))
        (\ x -> case x of
                  ImgEnt (ImgRef i n) -> Right (i, n)
                  _                   -> Left  x
        )
{-# INLINE theColImgRef #-}

theColColRef :: Prism' (ColEntry' ref) ref
theColColRef =
  prism ColEnt
        (\ x -> case x of
                  ColEnt i -> Right i
                  _        -> Left  x
        )
{-# INLINE theColColRef #-}

isColColRef
  , isColImgRef :: ColEntry' ref -> Bool

isColColRef (ColEnt{}) = True
isColColRef _          = False

isColImgRef = not . isColColRef

-- ----------------------------------------

newtype DirEntries' ref = DE (Seq ref)

deriving instance (Eq   ref) => Eq   (DirEntries' ref)
deriving instance (Ord  ref) => Ord  (DirEntries' ref)
deriving instance (Show ref) => Show (DirEntries' ref)

deriving instance Functor DirEntries'

instance IsEmpty (DirEntries' ref) where
  isempty (DE xs) = isempty xs
  {-# INLINE isempty #-}

instance Semigroup (DirEntries' ref) where
  DE xs <> DE ys = DE $ xs <> ys

instance Monoid (DirEntries' ref) where
  mempty  = DE mempty
  mappend = (<>)

instance (ToJSON ref) => ToJSON (DirEntries' ref) where
  toJSON (DE rs) = toJSON rs
  {-# INLINE toJSON #-}

instance (FromJSON ref) => FromJSON (DirEntries' ref) where
  parseJSON rs = DE <$> parseJSON rs
  {-# INLINE parseJSON #-}

mkDirEntries :: Seq ref -> DirEntries' ref
mkDirEntries = DE
{-# INLINE mkDirEntries #-}

isoDirEntries :: Iso' (DirEntries' ref) (Seq ref)
isoDirEntries = iso (\ (DE xs) -> xs) DE
{-# INLINE isoDirEntries #-}

addDirEntry :: ref -> DirEntries' ref -> DirEntries' ref
addDirEntry r (DE rs) = DE rs'
  where
    ! rs' = r Seq.<| rs
{-# INLINE addDirEntry #-}

delDirEntry :: (Eq ref) => ref -> DirEntries' ref -> DirEntries' ref
delDirEntry r (DE rs) =
  DE rs'
  where
    ! rs' = Seq.filter (/= r) rs
{-# INLINE delDirEntry #-}

delColEntry :: (Eq ref) => ref -> ColEntries' ref -> ColEntries' ref
delColEntry r =
    Seq.filter (\ ce -> ce ^. theColObjId /= r)
{-# INLINE delColEntry #-}


-- ----------------------------------------

newtype ColEntrySet' ref = CES (Set (ColEntry' ref))

type    ColEntrySet      = ColEntrySet' ObjId

deriving instance (Show ref) => Show (ColEntrySet' ref)

instance Ord ref => Semigroup (ColEntrySet' ref) where
  CES s1 <> CES s2 = CES $ s1 `S.union` s2

instance Ord ref => Monoid (ColEntrySet' ref) where
  mempty  = CES S.empty
  mappend = (<>)

instance IsEmpty (ColEntrySet' ref) where
  isempty (CES s) = S.null s

singletonColEntrySet :: ColEntry' ref -> ColEntrySet' ref
singletonColEntrySet = CES . S.singleton

fromListColEntrySet :: Ord ref => [ColEntry' ref] -> ColEntrySet' ref
fromListColEntrySet = CES . S.fromList

fromSeqColEntrySet :: Ord ref => ColEntries' ref -> ColEntrySet' ref
fromSeqColEntrySet cs = CES $ foldMap S.singleton cs

toListColEntrySet :: ColEntrySet' ref -> [ColEntry' ref]
toListColEntrySet (CES s) = S.toList s

toSeqColEntrySet :: ColEntrySet' ref -> ColEntries' ref
toSeqColEntrySet (CES s) = foldMap Seq.singleton s

memberColEntrySet :: Ord ref
                  => ColEntry' ref
                  -> ColEntrySet' ref -> Bool
memberColEntrySet ce (CES s) = ce `S.member` s

diffColEntrySet :: Ord ref
                => ColEntrySet' ref
                -> ColEntrySet' ref
                -> ColEntrySet' ref
CES s1 `diffColEntrySet` CES s2 = CES $ s1 `S.difference` s2

intersectColEntrySet :: Ord ref
                     => ColEntrySet' ref
                     -> ColEntrySet' ref
                     -> ColEntrySet' ref
CES s1 `intersectColEntrySet` CES s2 = CES $ s1 `S.intersection` s2

-- ----------------------------------------

type ObjIds = Set ObjId

singleObjId :: ObjId -> ObjIds
singleObjId = S.singleton

-- ----------------------------------------

hasAccessRights :: (MetaData -> Bool) -> ImgNode' a -> Bool
hasAccessRights p n =
  isCOL n && (p $ n ^. theColMetaData)

isWriteableCol
  , isSortableCol, isRemovableCol :: ImgNode' a -> Bool

isWriteableCol = hasAccessRights isWriteable
isSortableCol  = hasAccessRights isSortable
isRemovableCol = hasAccessRights isRemovable

-- ----------------------------------------
--
-- a ref into a collection
-- Nothing: the collection itself is referenced
-- Just i: the i-th entry is referenced

type ColRef' a   = (a, Maybe Int)
type ColRef      = ColRef' ObjId

-- ----------------------------------------

cColRef :: (ObjId -> a) -> (ObjId -> Int -> a) -> ColRef -> a
cColRef cref iref (i, p) =
  case p of
    Nothing  -> cref i
    Just pos -> iref i pos

-- ----------------------------------------