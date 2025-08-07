{-# LANGUAGE InstanceSigs #-}
module Data.ImgNode
       ( ImgNode'(..)
       , ImgParts
       , ImgPart
       , ImgRef'(..)
       , ImgRef
       , ColEntry'(..)
       , ColEntries'
       , ColEntryM'
       , ColEntriesM'
       , DirEntries'
       , mkImgParts
       , mkImgPart
       , mkColImgRef
       , mkColImgRef'
       , mkColImgRefM
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
       , colEntryM
       , colEntryM'
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
       , theColEntry
       , theColMeta
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

-- import           Control.Monad.Except

import           Data.MetaData
{-
                               ( MetaData
                               , metaDataAt

                               , fileMimeType
                               , fileName
                               , fileTimeStamp
                               , fileCheckSum
                               , metaCheckSum

                               , metaMimeType
                               , metaName
                               , metaTimeStamp

                               , isWriteable
                               , isSortable
                               , isRemovable
                               , isAUserCol
                               )
-}

import Data.Prim
{-
    ( Traversal',
      Lens',
      Field4(_4),
      Field3(_3),
      Field2(_2),
      Field1(_1),
      Ixed(ix),
      Alternative((<|>)),
      (.~),
      (#),
      filteredBy,
      filtered,
      (&),
      prism,
      (^.),
      iso,
      isoMapElems,
      FromJSON(parseJSON),
      ToJSON(toJSON),
      Iso',
      Prism',
      Map,
      Seq,
      Set,
      Text,
      ObjId,
      TimeStamp,
      CheckSum,
      Name,
      MimeType,
      isShowablePartMT,
      t'archive,
      t'collections )
 -}

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

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

deriving instance Functor ImgNode'

instance AsEmpty (ImgNode' ref) where
  _Empty = nearly emptyImg ise
    where
      ise (IMG _pts _md)       = True
      ise (DIR es _ts)         = isEmpty es
      ise (COL _md _im _be cs) = isEmpty cs
      ise (ROOT _d _c)         = False

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

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance AsEmpty ImgParts where
  _Empty :: Prism' ImgParts ()
  _Empty = nearly mempty (\ (ImgParts m) -> isEmpty m)

instance Semigroup ImgParts where
  (<>) :: ImgParts -> ImgParts -> ImgParts
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
  mempty :: ImgParts
  mempty  = ImgParts M.empty
  {-# INLINE mempty #-}

instance ToJSON ImgParts where
  toJSON :: ImgParts -> J.Value
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

data ImgRef' ref = ImgRef {_iref :: !ref, _iname ::  !Name}
type ImgRef      = ImgRef' ObjId

deriving instance (Eq   ref) => Eq   (ImgRef' ref)
deriving instance (Ord  ref) => Ord  (ImgRef' ref)
deriving instance Functor     ImgRef'
deriving instance Foldable    ImgRef'
deriving instance Traversable ImgRef'

instance (ToJSON ref) => ToJSON (ImgRef' ref) where
  toJSON :: ToJSON ref => ImgRef' ref -> J.Value
  toJSON (ImgRef i n) = J.toJSON (i, n)

instance (Show ref) => Show (ImgRef' ref) where
  show :: Show ref => ImgRef' ref -> String
  show (ImgRef r n) = show (r, n)

instance Monoid ref => AsEmpty (ImgRef' ref) where
  _Empty :: Monoid ref => Prism' (ImgRef' ref) ()
  _Empty = nearly (ImgRef mempty mempty)
                  (isEmpty . _iname)
  {-# INLINE _Empty #-}

emptyImgRef :: ImgRef
emptyImgRef = ImgRef mempty mempty
{-# INLINE emptyImgRef #-}

-- --------------------
--
-- extension of collection entries
-- to carry info about slideshow attributes
-- like duration to show, blend mode, blending duration, ...
--
-- the obvious solution is
--
-- > type ColEntriesM' ref = Seq (ColEntry' ref, MetaData)
--
-- the need for this occurs only for collections
-- presented as slideshows
-- but it introduces space overhead in all collections
--
-- the solution tried here is an introduction of new
-- constructors for the metadata fields
-- and a few isos and lenses to transform between
-- the entries in the col sequence and the old representation

data ColEntryM'   ref = IE  !(ImgRef' ref)
                      | CE  !ref
                      | IEM !(ImgRef' ref) MetaData
                      | CEM !ref           MetaData

type ColEntriesM' ref = Seq (ColEntryM' ref)

deriving instance Functor     ColEntryM'
deriving instance Foldable    ColEntryM'
deriving instance Traversable ColEntryM'

instance Show ref => Show (ColEntryM' ref) where
  show :: Show ref => ColEntryM' ref -> String
  show r
    | isEmpty m  = show i
    | otherwise  = show t
    where
      t@(i, m) = r ^. isoColEntryTuple

instance (ToJSON ref) => ToJSON (ColEntryM' ref) where
  toJSON :: ToJSON ref => ColEntryM' ref -> J.Value
  toJSON cs
    | isEmpty m = toJSON i
    | otherwise = toJSON t
    where
      t@(i, m) = cs ^. isoColEntryTuple

instance (FromJSON ref) => FromJSON (ColEntryM' ref) where
  parseJSON v =
    ( fmap toCE  . parseJSON $ v)   -- old style: a single ColEntry without MetaData
    <|>
    ( fmap toCEM . parseJSON $ v)   -- new style: a pair of (ColEntry, MetaData)
    where
      toCE :: ColEntry' ref -> ColEntryM' ref
      toCE r = isoColEntryTuple # (r, mempty)

      toCEM :: (ColEntry' ref, MetaData) -> ColEntryM' ref
      toCEM t = isoColEntryTuple # t

isoColEntryTuple :: Iso' (ColEntryM' ref) (ColEntry' ref, MetaData)
isoColEntryTuple = iso isoTo isoFrom
  where
    isoTo (IE  i)   = (ImgEnt i, mempty)
    isoTo (CE  c)   = (ColEnt c, mempty)
    isoTo (IEM i m) = (ImgEnt i, m)
    isoTo (CEM c m) = (ColEnt c, m)

    isoFrom (ImgEnt i, m)
      | isEmpty m   = IE  i
      | otherwise   = IEM i m

    isoFrom (ColEnt c, m)
      | isEmpty m   = CE  c
      | otherwise   = CEM c m
{-# INLINE isoColEntryTuple  #-}

mkColImgRefM :: ref -> Name -> ColEntryM' ref
mkColImgRefM i n = IE $ ImgRef i n
{-# INLINE mkColImgRefM #-}

theColEntry :: Lens' (ColEntryM' ref) (ColEntry' ref)
theColEntry = isoColEntryTuple . _1
{-# INLINE theColEntry #-}

theColMeta :: Lens' (ColEntryM' ref) MetaData
theColMeta = isoColEntryTuple . _2
{-# INLINE theColMeta #-}

colEntryM ::
  (ref -> Name -> a) ->
  (ref -> a) ->
  ColEntryM' ref ->
  a
colEntryM imgRef colRef r = r ^. theColEntry . to (colEntry imgRef colRef)
{-# INLINE colEntryM #-}

colEntryM' ::
  (ImgRef' ref -> a) ->
  (ref -> a) ->
  ColEntryM' ref ->
  a
colEntryM' imgRef colRef r = r ^. theColEntry . to (colEntry' imgRef colRef)
{-# INLINE colEntryM' #-}

-- --------------------

data ColEntry'   ref  = ImgEnt !(ImgRef' ref)
                      | ColEnt !ref

type ColEntries' ref = Seq (ColEntry' ref)

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

deriving instance (Eq   ref) => Eq   (DirEntries' ref)
deriving instance (Ord  ref) => Ord  (DirEntries' ref)
deriving instance (Show ref) => Show (DirEntries' ref)

deriving instance Functor     DirEntries'
deriving instance Foldable    DirEntries'
deriving instance Traversable DirEntries'

instance Semigroup (DirEntries' ref) where
  (<>) :: DirEntries' ref -> DirEntries' ref -> DirEntries' ref
  DE xs <> DE ys = DE $ xs <> ys
  {-# INLINE (<>) #-}

instance Monoid (DirEntries' ref) where
  mempty :: DirEntries' ref
  mempty  = DE mempty
  {-# INLINE mempty #-}

instance AsEmpty (DirEntries' ref) where
  _Empty :: Prism' (DirEntries' ref) ()
  _Empty = nearly (DE mempty) (\(DE s) -> isEmpty s)
  {-# INLINE _Empty #-}

instance (ToJSON ref) => ToJSON (DirEntries' ref) where
  toJSON :: ToJSON ref => DirEntries' ref -> J.Value
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
delDirEntry r (DE rs) =
  DE rs'
  where
    !rs' = Seq.filter (/= r) rs
{-# INLINE delDirEntry #-}

delColEntry :: (Eq ref) => ref -> ColEntries' ref -> ColEntries' ref
delColEntry r =
    Seq.filter (\ ce -> ce ^. theColObjId /= r)
{-# INLINE delColEntry #-}

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
