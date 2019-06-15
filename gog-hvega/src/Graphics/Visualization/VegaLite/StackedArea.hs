{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Graphics.Visualization.VegaLite.StackedArea
  (
    -- * Visualizations
    stackedAreaVsTime
    -- * Configuration Re-exports
  , ViewConfig
    -- * Hvega Re-exports
  , TimeUnit(..)
  )
where

import qualified Graphics.Visualization.GOG.Data
                                               as GG
import qualified Graphics.Visualization.VegaLite.Common
                                               as VC
import           Graphics.Visualization.VegaLite.Common
                                                ( ViewConfig(..) )

import qualified Data.Array                    as A
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

stackedAreaVsTime
  :: (Traversable f, Show k, A.Ix k)
  => T.Text -- ^ Title
  -> T.Text -- ^ group label
  -> T.Text -- ^ year label
  -> T.Text -- ^ amount label
  -> GG.DataRows f k -- ^data
  -> GV.TimeUnit
  -> VC.ViewConfig -- sizing information 
  -> Either T.Text GV.VegaLite
stackedAreaVsTime title gL tL aL dr tu vc = do
  _ <- GG.checkLabels
    dr
    [ (gL, [GG.StrField])
    , (tL, [GG.IntYearField, GG.DateTimeField])
    , (aL, [GG.CountField, GG.NumberField])
    ]
  let dat = VC.toHvegaData dr
      xEnc =
        GV.position GV.X [GV.PName tL, GV.PmType GV.Temporal, GV.PTimeUnit tu]
      yEnc     = GV.position GV.Y [GV.PName aL, GV.PmType GV.Quantitative]
      colorEnc = GV.color [GV.MName gL, GV.MmType GV.Nominal]
      enc      = xEnc . yEnc . colorEnc
      specs =
        [ GV.asSpec
            [ (GV.encoding . enc) []
            , GV.mark GV.Area [GV.MInterpolate GV.Monotone]
            ]
        ]
      configuration = GV.configure . VC.viewConfigAsHvega vc
  return $ GV.toVegaLite [GV.title title, GV.layer specs, dat, configuration []]
