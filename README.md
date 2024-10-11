Hinteractive: Interaction Fiction Game Engine
============================================

A game engine for creating text-based adventure games like Zork (under development).

Features:
* Basic game mechanics:
  * player command input;
  * text output;
  * game state management;
  * generic game objects of arbitrary (serializable) types;
  * object interactions;
  * container objects;
  * item objects (objects the player can pick up, store, carry, and use) (WIP);
  * description of locations and transitions.

Demo Game
---------

The demo game showcases the engine's core capabilities.

```bash
stack build
stack exec hinteractive-exe
```

Architecture
------------

The engine consists of two main components:
* Game mechanics ([hinteractive](https://github.com/graninas/hinteractive))
  Based on the Free monadic language `AdventureL`.
  This allows you to describe scenarios that will be executed over the game state.
  The game state is the state of all interactive objects and the player.
* Reactive graph of locations and transitions ([transition-graph](https://github.com/graninas/transition-graph))
  The graph serves as an eDSL for describing locations (graph nodes) and transitions between them (triggered by events or unconditionally).
  It allows embedding an external language (`AdventureL`) to control the graph.

The [aeson](http://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html) library is used for serializing the game state.

[Lenses](http://hackage.haskell.org/package/lens) are used for easier data type manipulation.

In the future, another major component is planned—a language model. This model will parse natural language commands (English) and convert them into operations on the game state.

AdventureL Game Scenario Language
---------------------------------

The `AdventureL` type is a Free monad type that describes available operations on the game state. Monadic scenarios are built from these operations and embedded into the game's locations (graph nodes).

An example scenario that prints "Hello, World" to the player, where `printMessage` is a monadic operation from the `AdventureL` language:

```haskell
helloWorld :: AdventureL ()
helloWorld = printMessage "Hello, World"
```

This type is based on a Generalized ADT (`AdventureLF`), whose constructors encode the available operations. It is a GADT because operations on objects have more complex syntax and semantics (specifically, they need to be serializable).

```haskell
-- | Free monadic game scenario language.
type AdventureL a = Free AdventureLF a

-- | Generalized ADT for the game mechanics language.
data AdventureLF next where
  -- | General game operations (input/output).
  GetUserInput :: (String -> next) -> AdventureLF next
  PrintMessage :: String  -> next  -> AdventureLF next

  -- | Inventory actions (stub: the Item type needs reworking).
  PutItem   :: Item -> next -> AdventureLF next
  DropItem  :: Item -> next -> AdventureLF next
  ListItems ::         next -> AdventureLF next

  -- | Actions on game object states.
  -- You can request an object's state and update it by its name.
  GetObjSt :: FromJSON a => ObjectName -> (a -> next) -> AdventureLF next
  PutObjSt :: ToJSON a   => ObjectName -> a -> next -> AdventureLF next
```

For convenience, the `AdventureLF` type is hidden, and "smart constructors" are provided in the `AdventureL` type:

```haskell
-- Player interaction:

-- Get user input from the console.
getUserInput :: AdventureL String
-- Print a message to the console.
printMessage :: String -> AdventureL ()

-- Inventory actions (require rework):

-- Add an item to the inventory.
putItem :: String -> AdventureL ()
-- Drop an item.
dropItem :: AdventureL ()
-- List items in the inventory.
listItems :: AdventureL ()
```

There are also operations for interacting with generic game objects: `getObject`, `getObject'`, and `putObject`. These operations have more complex signatures, so they will be described separately.

Gameplay involves transitioning between graph nodes and executing (interpreting) the scenarios in those nodes. When executed, the scenarios modify the game state in various ways.

The state of game objects is maintained at runtime using the `State` monad:

```haskell
-- State of all game objects
type ObjectStates = Map.Map String BSL.ByteString

-- Player's inventory
type Inventory = Map.Map String Item

-- Runtime (game state)
data Runtime = Runtime
  { _inventory    :: Map.Map String Item
  , _objectStates :: ObjectStates
  }

-- Interpreter type for AdventureL: a monad stack of State and IO.
type Interpreter a = StateT Runtime IO a
```

The `AdventureL` language is pure monadic.
Its interpreter is an impure monadic (working in the `IO` monad).

Game Object Mechanics
----------------------

Game objects are objects with mutable state. Their state can change either due to player actions or based on predefined scenarios.

The state of a game object is described in a separate type in each game. This type is unknown to the engine. The only current requirement is that it must be serializable.

Example:

```haskell
-- Serializable type - state of a mailbox (but not the object itself)
data MailboxSt = MailboxSt
  { _description :: String      -- ^ Text description of the mailbox
  , _container   :: Container   -- ^ "Container", which the mailbox is
  }
  deriving (Generic, ToJSON, FromJSON)
```

Here, `MailboxSt` is the state of a mailbox.
The type classes `Generic`, `ToJSON`, and `FromJSON` make it serializable.

Since the mailbox is a container, it has a `_container` field of the special `Container` type. Containers can be open or closed and can hold other objects:

```haskell
data ContainerState = Opened | Closed
  deriving (Generic, ToJSON, FromJSON)

-- Type describing a container.
-- Currently contains a stub for stored objects - the `Item` type.
data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }
  deriving (Generic, ToJSON, FromJSON)
```

Example:

```haskell
mailboxObj = MailboxSt
  { _description = "This is a small mailbox."
  , _container = Container Closed ["leaflet"]
  }
```

The type `MailboxSt` described above is just the state of the object, not the object itself. The current approach requires that for each such type, a "representative" ADT is specified, which allows for more convenient work with objects of that type and generic operations.

```haskell
data MailboxType = MailboxType
```

You also need to describe the operations that can be performed on instances of this object. The operations (or `Actions`) will be defined later. For example, a mailbox can be opened and closed because it is a container. Types for object operations will be described later.

The final generic game object will be described by a complex `Object` type, with a phantom parameter passing the "representative" type as `objType`:

```haskell
-- | Generic game object.
data Object objType objSt = Object
  { _name    :: ObjectName      -- ^ Name of the object (essentially, a key in the map of all game objects).
  , _state   :: objSt           -- ^ Object state.
  , _actions :: Actions objSt   -- ^ Available actions on the object.
  }
```

For the mailbox, `Object` will be parameterized by the following types:

`objType :: MailboxType`
`objSt   :: MailboxSt`

Since the `Actions` type is currently unserializable (it consists of functions and scenarios in the `AdventureL` monad), to bind an object's state to actions on it, you can't just retrieve `objSt` from the game state; you also need to know which `actions` are tied to this type. This is done using the `ToObject` type class and the "representative" `objType`:

```haskell
class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt
```

An example for the mailbox:

```haskell
instance ToObject MailboxType MailboxSt where
  object objSt = Object "mailbox" objSt $ Map.fromList
    [ ("open",  Action openContainer  onMailboxOpenSuccess  onMailboxOpenFail  )
    , ("close", Action closeContainer onOpenCloseSuccess onMailboxCloseFail )
    ]
```

Here, the list contains actions executed on specific commands (`"open"`, `"close"`). The actions consist of a main operation (`openContainer`, `closeContainer`) and success or failure handlers (`onMailboxOpenSuccess`, `onMailboxOpenFail`).

Finally, to work with generic objects in scenarios, there are `getObject`, `getObject'`, and `putObject` functions, which retrieve an object's state from the game state, deserialize it, and allow you to work with it in a scenario; or serialize the object's state back into the game state. These functions use the `ToObject` type class and the representative `objType` to deserialize the object to the correct `objSt` type:

```haskell
getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => ObjectName  -- ^ Object name as a key
  ->

 AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObjSt name id
  pure $ object objSt

getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType     -- ^ "Representative" type
  -> ObjectName  -- ^ Object name as a key
  -> AdventureL (Object objType objSt)
getObject' _ = getObject

putObject
  :: ToJSON objSt
  => ObjectName   -- ^ Object name as a key
  -> objSt        -- ^ Object state
  -> AdventureL ()
putObject name objSt = liftF $ PutObjSt name objSt ()
```

An example scenario that retrieves a mailbox and tells the player what’s inside:

```haskell
describeMailbox :: AdventureL ()
describeMailbox = do
  mailbox :: Mailbox <- getObject "mailbox"
  when showMailbox $ printMessage $ describeObject mailbox  -- A special operation that prints the object's _description.
```

In the future, it is planned to replace the representative `objType` with string type literals.

Actions
-------

TODO

Creating the Game (Locations, Transitions)
------------------------------------------

TODO
