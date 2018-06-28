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

data MailboxSt = MailboxSt
  { _description :: String     -- ^ Description
  , _container   :: Container  -- ^ Mailbox is a container
  }
  deriving (Generic, ToJSON, FromJSON)

makeFieldsNoPrefix ''MailboxSt

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
type Mailbox = Object MailboxType MailboxSt

mailboxSt = MailboxSt
  { _description = "This is a small mailbox."
  , _container = Container Closed ["leaflet"]
  }

instance ToObject MailboxType MailboxSt where
  object objSt = Object "mailbox" objSt $ Map.fromList
    [ ("open",  Action openContainer  onMailboxOpenSuccess  onMailboxOpenFail  )
    , ("close", Action closeContainer onOpenCloseSuccess onMailboxCloseFail )
    ]

onMailboxOpenSuccess :: MailboxSt -> AdventureL ()
onMailboxOpenSuccess mailbox = case mailbox ^. container.items of
  []    -> printMessage "Opened."
  items -> printMessage $ "Opening mailbox revealed " ++ intercalate ", " items

onMailboxOpenFail :: MailboxSt -> AdventureL ()
onMailboxOpenFail _ = printMessage "Mailbox already opened."

onOpenCloseSuccess :: MailboxSt -> AdventureL ()
onOpenCloseSuccess _ = printMessage "Closed."

onMailboxCloseFail :: MailboxSt -> AdventureL ()
onMailboxCloseFail _ = printMessage "Mailbox already closed."

-- Leaflet

type Message = String

leafletSt :: Message
leafletSt = "There are an amazing city in the east behind the high mountains"

data MessageType = MessageType
type Leaflet = Object MessageType Message

instance ToObject MessageType Message where
  object objSt = Object "leaflet" objSt $ Map.fromList
    [ ("read", Action Just readLeaflet (const (pure ())))
    ]

readLeaflet :: Message -> AdventureL ()
readLeaflet = printMessage
