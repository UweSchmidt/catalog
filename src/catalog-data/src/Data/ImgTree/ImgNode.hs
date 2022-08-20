module Data.ImgTree.ImgNode
       ( ImgNode'(..)
       , ImgNode
       , ImgNodeP
       , ImgParts
       , ImgPart
       , ImgRef'(..)
       , ImgRef
       , ColEntry'(..)
       , ColEntry
       , ColEntries'
       , ColEntries
       , DirEntries'
       , DirEntries
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
       , imgname
       , imgref
       , imgNodeRefs
       , isDIR
       , isIMG
       , isROOT
       , isCOL
       , isNUL
       , isemptyDIR
       , isColColRef
       , isColImgRef
       , colEntry
       , colEntry'
       , colNodeImgRefs
       , colNodeColRefs
       , isoImgParts
       , isoImgPartsMap
       , isoDirEntries
       , theParts
       , traverseParts
       , thePartNames'
       , thePartNames
       , thePartNamesI
       , theImgMeta
       , theImgName
       , theImgPart
       , theMimeType
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
       , filterDirEntries
       , filterColEntries
       , ObjIds
       , singleObjId
       , isWriteableCol
       , isSortableCol
       , isRemovableCol
       , isUserCol
       , ColRef'
       , ColRef
       , cColRef
       )
where

import           Control.Monad.Except

import           Data.MetaData
import           Data.Prim

import qualified Data.Aeson      as J
import qualified Data.Aeson.Key  as J
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

type ImgNode    = ImgNode'    ObjId
type ImgNodeP   = ImgNode'    Path

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

deriving instance Functor ImgNode'
{-
instance IsEmpty (ImgNode' ref) where
  isempty (IMG _pts _md)       = True
  isempty (DIR es _ts)         = isempty es
  isempty (COL _md _im _be cs) = isempty cs
  isempty (ROOT _d _c)         = False
-}

isemptyDIR :: ImgNode' ref -> Bool
isemptyDIR (DIR es _ts) = isempty es
isemptyDIR _            = False

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm md) = J.object
    [ "ImgNode"     J..= ("IMG" :: Text)
    , "parts"       J..= pm
    , "metadata"    J..= md
    ]
  toJSON (DIR rs ts) = J.object
    [ "ImgNode"     J..= ("DIR" :: Text)
    , "children"    J..= rs
    , "sync"        J..= ts
    ]
  toJSON (ROOT rd rc) = J.object
    [ "ImgNode"     J..= ("ROOT" :: Text)
    , J.fromText t'archive     J..= rd
    , J.fromText t'collections J..= rc
    ]
  toJSON (COL md im be es) = J.object $
    [ "ImgNode"    J..= ("COL" :: Text)
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
       case t :: Text of
         "IMG" ->
           IMG  <$> o J..: "parts"
                <*> o J..: "metadata"
         "DIR" ->
           DIR  <$> o J..: "children"
                <*> o J..:? "sync" J..!= mempty
         "ROOT" ->
           ROOT <$> o J..: J.fromText t'archive
                <*> o J..: J.fromText t'collections
         "COL" ->
           COL  <$> o J..: "metadata"
                <*> ((uncurry ImgRef <$>) <$> o J..:? "image" J..!= Nothing)
                <*> ((uncurry ImgRef <$>) <$> o J..:? "blog"  J..!= Nothing)
                <*> o J..: "entries"
         _ -> mzero

instance IsEmpty (ImgNode' ref) where
  isempty (IMG pm md) = isempty pm && isempty md
  isempty _           = False

instance Semigroup (ImgNode' ref) where
  n1 <> n2
    | isempty n1 = n2
    | otherwise  = n1

instance Monoid (ImgNode' ref) where
  mempty = emptyImg         -- TODO: remove this hack by a NULL node

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

-- --------------------
-- image node optics

imgNodeRefs :: Fold (ImgNode' ref) ref
imgNodeRefs = folding go
  where
    go (IMG _ _)        = []
    go (DIR de _)       = de ^.. traverse
    go (ROOT cr dr)     = [cr, dr]
    go (COL _ ir br cs) = ir ^.. traverse . imgref
                          <>
                          br ^.. traverse . imgref
                          <>
                          cs ^.. traverse . theColObjId

colNodeImgRefs :: Fold (ImgNode' ref) (ImgRef' ref)
colNodeImgRefs = folding go
  where
    go (COL _ ir br cs) = ir ^.. traverse
                          <>
                          br ^.. traverse
                          <>
                          cs ^.. traverse . theColImgRef
    go _                = []

colNodeColRefs :: Fold (ImgNode' ref) ref
colNodeColRefs = folding go
  where
    go (COL _ _ _ cs) = cs ^.. traverse . theColColRef
    go _                = []


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
                    ( MetaData
                    , Maybe (ImgRef' ref)
                    , Maybe (ImgRef' ref)
                    , ColEntries' ref
                    )
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

isNUL :: ImgNode' ref -> Bool
isNUL (IMG md pts) = isempty md && isempty pts
isNUL _            = False

-- TODO hack: add a NUL variant to ImgNode

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
  toJSON ips = toJSON $ ips ^. isoImgParts
  -- old
  -- toJSON (ImgParts pm) = toJSON . M.toList $ pm
  {-# INLINE toJSON #-}

instance FromJSON ImgParts where
  parseJSON x =
    (isoImgParts #) <$> parseJSON x          -- parse new: [ImgPart]
    <|>
    ImgParts . M.fromList <$> parseJSON x    -- parse old: Map Name ImgPart

mkImgParts :: [ImgPart] -> ImgParts
mkImgParts ps = isoImgParts # ps
{-# INLINE mkImgParts #-}

isoImgParts :: Iso' ImgParts [ImgPart]
isoImgParts =
  iso (\ (ImgParts pm) -> pm) ImgParts
  .
  isoMapElems (^. theImgName) -- (\ (IPN n _ _) -> n)
{-# INLINE isoImgParts #-}

isoImgPartsMap :: Iso' ImgParts (Map Name ImgPart)
isoImgPartsMap = iso (\ (ImgParts pm) -> pm) ImgParts

traverseParts :: Traversal' ImgParts ImgPart
traverseParts = isoImgParts . traverse
{-# INLINE traverseParts #-}

thePartNames' :: (MimeType -> Bool) -> Traversal' ImgParts Name
thePartNames' typTest =
  traverseParts . filteredBy (theMimeType . filtered typTest) . theImgName
{-# INLINE thePartNames' #-}

-- images with 1 of the given types can be rendered
thePartNamesI :: Traversal' ImgParts Name
thePartNamesI = thePartNames' isShowablePartMT
{-# INLINE thePartNamesI #-}

thePartNames :: Traversal' ImgParts Name
thePartNames = thePartNames' (const True)
{-# INLINE thePartNames #-}

-- ----------------------------------------
--
-- new ImgPart datatype
-- MetaData is used for all attributes of a part
-- not only mime type, timestamp and checksum, but also
-- geometry, orientation, ratings, ...

newtype ImgPart = IPM MetaData

deriving instance Show ImgPart

instance FromJSON ImgPart where
  parseJSON x = IPM <$> J.parseJSON x

instance ToJSON ImgPart where
  toJSON (IPM md) = toJSON md


mkImgPart :: Name -> MimeType -> ImgPart
mkImgPart n t =
  IPM mempty & theImgName .~ n
             & theMimeType .~ t
{-# INLINE mkImgPart #-}

theImgMeta :: Lens' ImgPart MetaData
theImgMeta k (IPM md) = IPM <$> k md
{-# INLINE theImgMeta #-}

theImgName :: Lens' ImgPart Name
theImgName = theImgMeta . metaDataAt fileName . metaName
{-# INLINE theImgName #-}

theMimeType :: Lens' ImgPart MimeType
theMimeType = theImgMeta . metaDataAt fileMimeType . metaMimeType
{-# INLINE theMimeType #-}

theImgTimeStamp :: Lens' ImgPart TimeStamp
theImgTimeStamp = theImgMeta . metaDataAt fileTimeStamp . metaTimeStamp
{-# INLINE theImgTimeStamp #-}

theImgCheckSum :: Lens' ImgPart CheckSum
theImgCheckSum = theImgMeta . metaDataAt fileCheckSum . metaCheckSum
{-# INLINE theImgCheckSum #-}

-- ----------------------------------------

data ImgRef' ref = ImgRef {_iref :: !ref, _iname :: !Name}
type ImgRef      = ImgRef' ObjId

deriving instance (Eq   ref) => Eq   (ImgRef' ref)
deriving instance (Ord  ref) => Ord  (ImgRef' ref)
deriving instance Functor     ImgRef'
deriving instance Foldable    ImgRef'
deriving instance Traversable ImgRef'

instance (ToJSON ref) => ToJSON (ImgRef' ref) where
  toJSON (ImgRef i n) = J.toJSON (i, n)

instance (Show ref) => Show (ImgRef' ref) where
  show (ImgRef r n) = show (r, n)

instance IsEmpty (ImgRef' ref) where
  isempty = isempty . _iname

emptyImgRef :: ImgRef
emptyImgRef = ImgRef mempty mempty

imgref :: Lens' (ImgRef' ref) ref
imgref k (ImgRef i n) = fmap (\ new -> ImgRef new n) (k i)

imgname :: Lens' (ImgRef' ref) Name
imgname k (ImgRef i n) = ImgRef i <$> k n

-- --------------------

data ColEntry'   ref  = ImgEnt !(ImgRef' ref)
                      | ColEnt !ref

type ColEntries' ref  = Seq (ColEntry' ref)

type ColEntry         = ColEntry'   ObjId
type ColEntries       = ColEntries' ObjId

deriving instance (Eq   ref) => Eq   (ColEntry' ref)
deriving instance (Ord  ref) => Ord  (ColEntry' ref)
deriving instance Functor     ColEntry'
deriving instance Foldable    ColEntry'
deriving instance Traversable ColEntry'

instance (Show ref) => Show (ColEntry' ref) where
  show = colEntry' show show

instance (ToJSON ref) => ToJSON (ColEntry' ref) where
  toJSON (ImgEnt (ImgRef i n)) = J.object
    [ "ColEntry"  J..= ("IMG" :: Text)
    , "ref"       J..= i
    , "part"      J..= n
    ]

  toJSON (ColEnt i) = J.object
    [ "ColEntry"  J..= ("COL" :: Text)
    , "ref"       J..= i
    ]

instance (FromJSON ref) => FromJSON (ColEntry' ref) where
  parseJSON = J.withObject "ColEntry" $ \ o ->
    do t <- o J..: "ColEntry"
       case t :: Text of
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

theColImgRef :: Prism' (ColEntry' ref) (ImgRef' ref)
theColImgRef =
  prism ImgEnt
        (\ x -> case x of
                  ImgEnt ir -> Right ir
                  _         -> Left  x
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

isColColRef ColEnt{} = True
isColColRef _        = False

isColImgRef = not . isColColRef

-- ----------------------------------------

newtype DirEntries' ref = DE (Seq ref)

type    DirEntries      = DirEntries' ObjId

deriving instance (Eq   ref) => Eq   (DirEntries' ref)
deriving instance (Ord  ref) => Ord  (DirEntries' ref)
deriving instance (Show ref) => Show (DirEntries' ref)

deriving instance Functor     DirEntries'
deriving instance Foldable    DirEntries'
deriving instance Traversable DirEntries'

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
    !rs' = r Seq.<| rs
{-# INLINE addDirEntry #-}

delDirEntry :: (Eq ref) => ref -> DirEntries' ref -> DirEntries' ref
delDirEntry r = filterDirEntries (/= r)
{-# INLINE delDirEntry #-}

delColEntry :: (Eq ref) => ref -> ColEntries' ref -> ColEntries' ref
delColEntry r = filterColEntries (\ ce -> ce ^. theColObjId /= r)
{-# INLINE delColEntry #-}

filterDirEntries :: (ref -> Bool)
                 -> DirEntries' ref -> DirEntries' ref
filterDirEntries p des = des & isoDirEntries %~ Seq.filter p
{-# INLINE filterDirEntries #-}

filterColEntries :: (ColEntry' ref -> Bool)
                 -> ColEntries' ref -> ColEntries' ref
filterColEntries = Seq.filter
{-# INLINE filterColEntries #-}

-- ----------------------------------------

type ObjIds = Set ObjId

singleObjId :: ObjId -> ObjIds
singleObjId = S.singleton

-- ----------------------------------------

hasAccessRights :: (MetaData -> Bool) -> ImgNode' a -> Bool
hasAccessRights p n =
  isCOL n && p (n ^. theColMetaData)

isWriteableCol
  , isSortableCol
  , isRemovableCol
  , isUserCol :: ImgNode' a -> Bool

isWriteableCol = hasAccessRights isWriteable
isSortableCol  = hasAccessRights isSortable
isRemovableCol = hasAccessRights isRemovable
isUserCol      = hasAccessRights isAUserCol

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
