module Visualization.GOG where

import qualified Data.Array as A
--import qualaified Data.Map as M
import qualified Data.Text as T

-- | type to store a single value, either text, numerical or temporal
data ItemValue where
  Str :: Text -> ItemValue
  Number :: Num a => a -> ItemValue
  Time :: DateTime -> ItemValue
  
--data IndexedItem k = IndexedItem { itemIndex :: k, itemValue :: ItemValue }

-- | A row of data is a set of values with some way of knowing which are in which column
-- The index, k, represents the column.
data IndexedRow k where
  dataRow :: A.Ix k => A.Array k Item 

-- | A data source is a collection of indexed rows
data DataSource f k where
  DataSource :: (A.Ix k, Traversable f) => f (IndexedRow k) -> DataSource f k

instance Semigroup (f (IndexedRow k)) => Semigroup (DataSource f k) where
  (DataSource c1) <> (DataSource c2) = DataSource (c1 <> c2)

instance Monoid (f (IndexedRow k)) => Monoid (DataSource f k) where
  mempty = DataSource mempty
  mappend = (<>)

type IndexLabel k = (k -> T.Text)
-- a GOG description has rows of data with types
-- visual encodings of that data (position, color)
-- expressions of those encodings in a visualization

type Encoding 

{-
-- | Any specific source will need a function to 
type SourceHandler s f k = s -> DataSource f k
-}


  


