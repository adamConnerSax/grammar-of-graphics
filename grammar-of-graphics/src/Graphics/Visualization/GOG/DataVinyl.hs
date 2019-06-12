{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Graphics.Visualization.GOG.DataVinyl
  (
    -- * Field Types 
    FieldType(..)
  , FieldValue(..)
    -- * FieldIndex and helpers
  , FieldIndex(..)
  , labelAt
  , indexFor
  , changeLabel
  -- * DataRows
  , DataRows(..)
  , checkLabels
  , buildDataRows

--  , toHvegaData
  , strLoader
  , countLoader
  , numLoader
  , dateTimeLoader
  , intYearLoader
  , boolLoader
    -- * Re-exports
  )
where

import           Control.Arrow                  ( second )
--import           Control.Lens                  as L
import qualified Data.Array                    as A

import qualified Data.Map                      as M
--import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time

import           Data.Kind                      ( Type )
import           GHC.TypeLits                   ( Symbol )
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.Derived            as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Frames                        as F
import qualified Frames.Melt                   as F
import qualified Frames.RecF                   as F

{-
-- | Enum to flag which type is being held
--data FieldType = StrField | CountField | NumberField | DateTimeField | IntYearField | BooleanField deriving (Show, Enum, Bounded, Ord, Eq)
-- | type to store a single value, either text, numerical or temporal
data FieldValue where
  Str ::Text -> FieldValue
  Count ::Int -> FieldValue
  Number ::Double -> FieldValue
  DateTime ::Time.LocalTime -> FieldValue
  IntYear ::Int -> FieldValue
  Boolean ::Bool -> FieldValue deriving (Eq, Show)


-- don't expose this constructor!!
data FieldLoader a = FieldLoader { flType :: FieldType, fLoader :: a -> FieldValue }

strLoader :: (a -> T.Text) -> FieldLoader a
strLoader f = FieldLoader StrField (Str . f)

countLoader :: (a -> Int) -> FieldLoader a
countLoader f = FieldLoader NumberField (Count . f)

numLoader :: (a -> Double) -> FieldLoader a
numLoader f = FieldLoader NumberField (Number . f)

dateTimeLoader :: (a -> Time.LocalTime) -> FieldLoader a
dateTimeLoader f = FieldLoader DateTimeField (DateTime . f)

intYearLoader :: (a -> Int) -> FieldLoader a
intYearLoader f = FieldLoader IntYearField (IntYear . f)

boolLoader :: (a -> Bool) -> FieldLoader a
boolLoader f = FieldLoader BooleanField (Boolean . f)

{-
itemFieldType :: FieldValue -> FieldType
itemFieldType (Str      _) = StrField
itemFieldType (Count    _) = CountField
itemFieldType (Number   _) = NumberField
itemFieldType (DateTime _) = DateTimeField
itemFieldType (IntYear  _) = IntYearField
itemFieldType (Boolean  _) = BooleanField
-}
-}

type Row rs = V.Rec V.ElField rs

getField :: forall t rs a s . (t ~ '(s, a), t F.∈ rs) => Row rs -> a
getField = F.rgetField @t

{-
-- | type to represent the Text version of column names and map to the index
data FieldIndex k where
  FieldIndex ::A.Ix k => M.Map T.Text k ->  A.Array k T.Text -> FieldIndex k

labelAt :: A.Ix k => FieldIndex k -> k -> T.Text
labelAt (FieldIndex _ fa) k = fst $ fa A.! k

fieldTypeAt :: A.Ix k => FieldIndex k -> k -> FieldType
fieldTypeAt (FieldIndex _ fa) k = snd $ fa A.! k

indexFor :: (Show k, A.Ix k) => FieldIndex k -> T.Text -> Either T.Text k
indexFor (FieldIndex indexMap _) l =
  maybe
      (  Left
      $  "Couldn't find label=\""
      <> l
      <> "\" in "
      <> (T.pack $ show indexMap)
      )
      Right
    $ M.lookup l indexMap
-}

{-
changeLabel
  :: forall x y rs. (V.KnownField x
                    ,V.KnownField y
                    ,V.Snd x ~ V.Snd y
                    , ElemOf rs x
                     V => T.Text -> T.Text -> FieldIndex k -> Either T.Text (FieldIndex k)
changeLabel old new fi@(FieldIndex indexMap labelTypeArray) = do
  index <- indexFor fi old
  let newIndexMap    = M.insert new index (M.delete old indexMap)
      (_, fieldType) = labelTypeArray A.! index
      newLTA         = labelTypeArray A.// [(index, (new, fieldType))]
  return $ FieldIndex newIndexMap newLTA


fieldIndexFromLabeledTypes :: [(T.Text, FieldType)] -> FieldIndex Int
fieldIndexFromLabeledTypes labelsAndTypes =
  let labelTypeArray =
          A.listArray (0, length labelsAndTypes - 1) labelsAndTypes
      indexMap =
          M.fromList $ fmap (\(k, (l, _)) -> (l, k)) $ A.assocs labelTypeArray
  in  FieldIndex indexMap labelTypeArray

data DataRows f k where
  DataRows ::(A.Ix k, Traversable f) => FieldIndex k -> f (A.Array k FieldValue) -> DataRows f k

checkLabels
  :: Show k => DataRows f k -> [(T.Text, [FieldType])] -> Either Text [()]
checkLabels (DataRows fi _) ls = do
  let f :: (T.Text, [FieldType]) -> Either T.Text ()
      f (t, fts) = do
        index <- indexFor fi t
        let ft = fieldTypeAt fi index
        if ((fts == []) || (elem ft fts))
          then Right ()
          else
            Left
            $  t
            <> " has fieldType="
            <> (T.pack $ show ft)
            <> " which is not in "
            <> (T.pack $ show fts)
  traverse f ls


foldIndexedFieldInRows
  :: (Foldable f, A.Ix k)
  => FL.Fold FieldValue x
  -> k
  -> FL.Fold (DataRows f k) x
foldIndexedFieldInRows ff k = FL.premap ((A.! k) . rows) ff

foldLabeledFieldInRows
  :: (Foldable f, A.Ix k)
  => FieldIndex k
  -> FL.Fold FieldValue x
  -> T.Text
  -> Either T.Text (FL.Fold (DataRows f k) x)
foldLabeledFieldInRows fi ff l = do
  k <- indexFor fi l
  return $ foldIndexedFieldInRows ff k

labeledFieldMinMax
  :: (Foldable f, A.Ix k)
  -> T.Text
  -> DataRows f k
  -> Either T.Text (FieldValue, FieldValue)
labeledFieldMinMax l (DataRows fi rows) = do
  k <- indexFor fi l
  let ft = fieldTypeAt fi k
  _ <- if (elem ft [CountField, NumberField, DateTimeField, IntYearField])
    then Right ()
    else
      Left
      $  l
      <> " of type "
      <> (T.pack $ show ft)
      <> " is not an ordered field type."
-}

buildDataRows
  :: (Functor f, F.StripFieldNames rs, V.RMap (V.Unlabeled rs))
  => F.Rec ((->) a) (V.Unlabeled rs)
  -> f a
  -> f (Row rs)
buildDataRows dataLoaders dat =
  fmap (\a -> F.withNames $ V.rmap (\f -> V.Identity (f a)) dataLoaders) dat


