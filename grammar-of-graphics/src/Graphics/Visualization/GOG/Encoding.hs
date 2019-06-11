{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Graphics.Visualization.GOG.Encoding
  ()
where

import qualified Graphics.Visualization.GOG.Data
                                               as GG

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data AxisType = X | Y | X2 | Y2 | Longitude | Latitude | Longitude2 | Latitude2 deriving (Show, Enum, Eq, Ord)
data FacetType = Row | Column deriving (Show, Enum, Eq, Ord)
data MeasurementType = Nominal | Ordinal | Quantitative | Temporal | GeoFeature

data Encoding k where
  Position::AxisType -> k -> MeasurementType -> Encoding k
  Color::k -> Encoding k
  Size::k -> Encoding k
  Fill::k -> Encoding k
  Stroke::k -> Encoding k
  Opacity::k -> Encoding k
  Shape::k -> Encoding k
  Text::k -> Encoding k
  Order::k -> Encoding k
  Facet::FacetType -> k -> Encoding k
  Detail::k -> Encoding k

