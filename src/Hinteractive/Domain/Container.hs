{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hinteractive.Domain.Container where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Either              (Either)
import qualified Data.Map                 as Map
import           Data.Maybe               (Maybe (..))
import           GHC.Generics             (Generic)

import           Hinteractive.Domain.Item

data ContainerState = Opened | Closed
  deriving (Generic, ToJSON, FromJSON)

data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }
  deriving (Generic, ToJSON, FromJSON)
