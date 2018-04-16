module AdvGame.Runtime where

import           Control.Monad             (mapM_)
import           Control.Monad.Free        (Free (..), foldFree, liftF)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, get, put)
import           Data.Aeson                (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Map                  as Map

import           AdvGame.Lang
import           Lib                       (Event)

type ObjectStates = Map.Map String BSL.ByteString

data Runtime = Runtime
  { _inventory    :: Map.Map String Item
  , _objectStates :: ObjectStates
  }

type Interpreter a = StateT Runtime IO a

interpret :: AdventureLF s -> Interpreter s
interpret (GetUserInput nextF) = do
  input <- lift $ putStr "> " >> getLine
  pure $ nextF input
interpret (PrintMessage s next)  = do
  lift $ putStrLn s
  pure next
interpret (PutItem s next) = error "Not implemented."
interpret (DropItem s next) = error "Not implemented."
interpret (ListItems next) = do
  Runtime inv _ <- get
  mapM_ (lift . putStrLn . snd) $ Map.toList inv
  pure next

interpret (GetObj name nextF) = do
  Runtime _ objs <- get
  case Map.lookup name objs of
    Nothing  -> error $ "Object " ++ name ++ " not found."
    Just obj -> case decode obj of
      Nothing -> error $ "Object " ++ name ++ " failed to decode."
      Just r  -> pure $ nextF r

interpret (PutObj name objSt next) = do
  Runtime inv objs <- get
  put $ Runtime inv $ Map.insert name (encode objSt) objs
  pure next

run :: AdventureL (Event, s) -> Interpreter (Event, s)
run = foldFree interpret
