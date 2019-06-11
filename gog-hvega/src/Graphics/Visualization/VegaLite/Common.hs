{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Graphics.Visualization.VegaLite.Common
  (
    -- * Helper Types    
    TimeEncoding(..)
  , Scaling(..)
  , ViewConfig(..)
    -- * helpers
  , intYear
  , timeField
  , viewConfigAsHvega
    -- * GOG 
  , toHvegaData
  )
where

import qualified Graphics.Visualization.GOG.Data as GG
import qualified Graphics.Vega.VegaLite        as GV

import           Control.Arrow                  ( second )
import qualified Control.Foldl                 as FL
import qualified Data.Array                    as A
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time

--import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

data Scaling = Default | DataMinMax deriving (Eq)

data ViewConfig = ViewConfig { vcWidth :: Double, vcHeight :: Double, vcPadding :: Double }

viewConfigAsHvega :: ViewConfig -> GV.BuildLabelledSpecs
viewConfigAsHvega (ViewConfig w h p) =
  GV.configuration (GV.View [GV.ViewWidth w, GV.ViewHeight h])
    . GV.configuration (GV.Padding $ GV.PSize p)

data TimeEncoding a = TimeEncoding { toStr :: a -> Text, timeFormat :: Text, timeUnit :: GV.TimeUnit }

-- helpers for time encoding
intYear :: TimeEncoding Int
intYear = TimeEncoding (T.pack . show) "%Y" GV.Year

timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStrF _ _) x = GV.Str $ toStrF x

toHvegaDataValue :: GG.FieldValue -> GV.DataValue
toHvegaDataValue (GG.Str      x) = GV.Str x
toHvegaDataValue (GG.Count    x) = GV.Number $ realToFrac x
toHvegaDataValue (GG.Number   x) = GV.Number x
toHvegaDataValue (GG.Boolean  x) = GV.Boolean x
toHvegaDataValue (GG.IntYear  x) = GV.Str $ T.pack $ show x
toHvegaDataValue (GG.DateTime x) = GV.Str $ T.pack $ show x -- yyyy-mm-dd hh:mm:ss  

-- add parsing info when required
hvegaDataType :: GG.FieldType -> Maybe GV.DataType
hvegaDataType GG.IntYearField  = Just $ GV.FoDate "%Y"
hvegaDataType GG.DateTimeField = Just $ GV.FoDate "%Y-%mm-%dd %H:%M:%S"
hvegaDataType _             = Nothing

hvegaParseList :: GG.FieldIndex k -> [(T.Text, GV.DataType)]
hvegaParseList (GG.FieldIndex _ labelTypeArray) =
  let f (a, mb) = (,) <$> pure a <*> mb
  in  catMaybes $ fmap (f . (\(_, (l, ft)) -> (l, hvegaDataType ft))) $ A.assocs
        labelTypeArray

toHvegaDataRow :: GG.FieldIndex k -> A.Array k GG.FieldValue -> [GV.DataRow]
toHvegaDataRow (GG.FieldIndex _ labelTypeArray) =
  flip GV.dataRow []
    . fmap (\(k, fv) -> (fst $ labelTypeArray A.! k, toHvegaDataValue fv))
    . A.assocs

toHvegaData :: GG.DataRows f k -> GV.Data
toHvegaData (GG.DataRows fi rows) =
  GV.dataFromRows [GV.Parse $ hvegaParseList fi]
    $ concat
    $ FL.fold FL.list
    $ fmap (toHvegaDataRow fi) rows



