{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZorkLike.Objects where

import           Control.Lens      ((.~), (^.))
import           Control.Lens.TH   (makeFieldsNoPrefix)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Either       (Either)
import           Data.List         (intercalate)
import qualified Data.Map          as Map
import           Data.Maybe        (Maybe (..))
import           GHC.Generics      (Generic)

import           Hinteractive
import           Hinteractive.Lens

data MailboxObj = MailboxObj
  { _description :: String
  , _container   :: Container
  }
  deriving (Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''MailboxObj

-- TODO: move to engine
openContainer :: HasContainer obj Container => obj -> Maybe obj
openContainer obj = case obj ^. container.state of
  Opened -> Nothing
  Closed -> Just $ container.state .~ Opened $ obj

closeContainer :: HasContainer obj Container => obj -> Maybe obj
closeContainer obj = case obj ^. container.state of
  Closed -> Nothing
  Opened -> Just $ container.state .~ Closed $ obj

describeObject :: HasDescription objSt String => Object objType objSt -> String
describeObject (Object _ objSt _) = objSt ^. description

---------- Concrete objects

data MailboxType = MailboxType
type Mailbox = Object MailboxType MailboxObj

mailboxObj = MailboxObj
  { _description = "This is a small mailbox."
  , _container = Container Closed ["leaflet"]
  }

instance ToObject MailboxType MailboxObj where
  object objSt = Object "mailbox" objSt $ Map.fromList
    [ ("open",  Action openContainer  onOpenMailboxSuccess  onMailboxOpenFail  )
    , ("close", Action closeContainer onCloseMailboxSuccess onMailboxCloseFail )
    ]

onOpenMailboxSuccess :: MailboxObj -> AdventureL ()
onOpenMailboxSuccess mailbox = case mailbox ^. container.items of
  []    -> printMessage "Opened."
  items -> printMessage $ "Opening mailbox revealed " ++ intercalate ", " items

onMailboxOpenFail :: MailboxObj -> AdventureL ()
onMailboxOpenFail _ = printMessage "Mailbox already opened."

onCloseMailboxSuccess :: MailboxObj -> AdventureL ()
onCloseMailboxSuccess _ = printMessage "Closed."

onMailboxCloseFail :: MailboxObj -> AdventureL ()
onMailboxCloseFail _ = printMessage "Mailbox already closed."
