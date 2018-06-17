{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hinteractive.Domain.Container where

import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Either              (Either)
import qualified Data.Map                 as Map
import           Data.Maybe               (Maybe (..))
import           GHC.Generics             (Generic)

import           Hinteractive.Domain.Item (Item)

-- This module describes an interactive objects that can be containers.

-- | The state of the container.
-- Containers can be serialized to and from JSON.
data ContainerState = Opened | Closed
  deriving (Generic, ToJSON, FromJSON)

-- | The type describing a container.
-- It currenlty has dependency from `Item` that is just a dummy type.
data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }
  deriving (Generic, ToJSON, FromJSON)
