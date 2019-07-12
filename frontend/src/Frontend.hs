{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Data.Map (Map)
import qualified Data.Map as Map

import Common.Api
import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      i <- inputElement def
      b <- button "Create accumulator"
      let createAccumulator = current (value i) <@ b
          f _ = foldDyn (:) [] createAccumulator
          accumulatorCreated = pushAlways f createAccumulator
          insertAbove :: a -> Map Int a -> Map Int a
          insertAbove v m = Map.insert k v m
            where k = case Map.maxViewWithKey m of
                    Nothing -> 1
                    Just ((oldK, _), _) -> succ oldK
      accumulators <- foldDyn insertAbove mempty accumulatorCreated
      display $ joinDynThroughMap accumulators
      pure ()
  }
