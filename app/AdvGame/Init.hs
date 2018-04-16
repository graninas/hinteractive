module AdvGame.Init where

import           Control.Lens         ((.~), (^.))
import           Control.Lens.TH      (makeClassy, makeFieldsNoPrefix,
                                       makeLenses)
import           Data.Either          (Either)
import           Data.List            (intercalate)
import qualified Data.Map             as Map
import           Data.Maybe           (Maybe (..))
import           GHC.Generics         (Generic)
import           Data.Aeson           (encode)

import           AdvGame.Runtime
import           AdvGame.Objects

inititalState :: Runtime
inititalState = Runtime
  { _inventory = Map.empty
  , _objectStates = initialObjectStates
  }

initialObjectStates :: ObjectStates
initialObjectStates = Map.fromList
  [ ("mailbox", encode mailboxObj)
  ]
