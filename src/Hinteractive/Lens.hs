{-# LANGUAGE TemplateHaskell #-}

module Hinteractive.Lens where

import           Control.Lens.TH               (makeFieldsNoPrefix)

import           Hinteractive.Domain.Container (Container)
import           Hinteractive.Domain.Object    (Object)

makeFieldsNoPrefix ''Container
makeFieldsNoPrefix ''Object
