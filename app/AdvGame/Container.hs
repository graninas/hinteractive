{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module AdvGame.Container where

import           Control.Lens    ((.~), (^.))
import           Control.Lens.TH (makeClassy, makeFieldsNoPrefix, makeLenses)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Either     (Either)
import qualified Data.Map        as Map
import           Data.Maybe      (Maybe (..))
import           GHC.Generics    (Generic)

import           AdvGame.Lang

data ContainerState = Opened | Closed
  deriving (Generic, ToJSON, FromJSON)

data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }
  deriving (Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''Container
