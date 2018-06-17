Hinteractive, v0.1
===================

Движок для создания текстовых приключенческих игр а-ля Zork (в разработке).

Возможности:
* Базовая механика игры:
* * ввод команд игрока;
* * вывод текстовой информации;
* * состояние игры;
* * генеричные игровые объекты произвольных (сериализуемых) типов;
* * действия над объектами;
* * объекты-контейнеры;
* * объекты-предметы (объекты, которые игрок может брать, хранить, переносить и использовать) (WIP);
* * описание локаций и переходов.

Демонстрационная игра
---------------------

В игре показаны основные возможности движка.

```bash
stack build
stack exec hinteractive-exe
```

Архитектура
-----------

Движок состоит из двух базовых частей:
* Игровая механика ([hinteractive](https://github.com/graninas/hinteractive))
  Основана на Free-монадическом языке `AdventureL`.
  Он позволяет описывать сценарии, которые будут исполняться над состоянием игры.
  Состояние игры - это состояние всех интерактивных объектов в ней и игрока.
* Реактивный граф локаций и переходов ([transition-graph](https://github.com/graninas/transition-graph))
  Граф представляет собой eDSL для описания локаций (узлов графа) и переходов между ними (по событию или без).
  Позволяет внедрить внешний язык (`AdventureL`), который будет управлять этим графом.

Для сериализации состояния игры используется библиотека [aeson](http://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html).

Для более удобной работы с типами данных используются линзы [lens](http://hackage.haskell.org/package/lens).

В будущем планируется добавить еще один большой компонент - языковую модель.
Эта модель позволит парсить команды на естественном языке (English)
и превращать их в операции над состоянием игры.

Язык игровых сценариев AdventureL
---------------------------------

Тип `AdventureL` - это Free-монадический тип, описывающий доступные операции
над состоянием игры. Из операций составляются монадические сценарии и внедряются
в локации игры (узлы графа).

Пример такого сценария, печатающий игроку "Hello, World", где `printMessage` -
монадическая операция из языка `AdventureL`:

```haskell
helloWorld :: AdventureL ()
helloWorld = printMessage "Hello, World"
```

Этот тип основан на Generalized ADT-типе `AdventureLF`, конструкторы которого кодируют
доступные операции. Тип является GADT потому, что операции работы с объектами
имеют более расширенный синтаксис и семантику (а именно - должны быть сериализуемы).

```haskell
-- | Free-монадический язык игровых сценариев.
type AdventureL a = Free AdventureLF a

-- | Generalized ADT для языка игровой механики.
data AdventureLF next where
  -- | Общие игровые операции (ввод-вывод).
  GetUserInput :: (String -> next) -> AdventureLF next
  PrintMessage :: String  -> next  -> AdventureLF next

  -- | Действия с инвентарем (заглушка: тип Item должен быть сделан иначе).
  PutItem   :: Item -> next -> AdventureLF next
  DropItem  :: Item -> next -> AdventureLF next
  ListItems ::         next -> AdventureLF next

  -- | Действия с состоянием игровых объектов.
  -- Можно запросить состояние объекта и сохранить новое по имени объекта.
  GetObjSt :: FromJSON a => ObjectName -> (a -> next) -> AdventureLF next
  PutObjSt :: ToJSON a   => ObjectName -> a -> next -> AdventureLF next
```

Для удобства тип `AdventureLF` скрыт, а в качестве интерфейса предоставляются
"умные конструкторы" в типе `AdventureL`:

```haskell
-- Взаимодействие с игроком:

-- Получить ввод пользователя с консоли.
getUserInput :: AdventureL String
-- Напечатать сообщение в консоль.
printMessage :: String -> AdventureL ()

-- Действия с инвентарем (требуют переработки):

-- Положить предмет в инвентарь.
putItem :: String -> AdventureL ()
-- Выбросить предмет.
dropItem :: String -> AdventureL ()
-- Перечислить предметы в инвентаре.
listItems :: AdventureL ()
```

Также имеются операции взаимодействия с генеричными игровыми объектами: `getObject`, `getObject'`, `putObject`.
Но эти операции имеют более сложную сигнатуру, поэтому будут описаны отдельно.

Процесс игры - это переход между узлами графа и исполнение (интерпретация) сценариев в этих узлах.
При исполнении сценарии изменяют состояние игры тем или иным способом.

Состояние игровых объектов хранится в рантайме интерпретатора при помощи монады `State`:

```haskell

-- Состояние всех игровых объектов
type ObjectStates = Map.Map String BSL.ByteString

-- Инвентарь игрока
type Inventory = Map.Map String Item

-- Рантайм (состояние игры)
data Runtime = Runtime
  { _inventory    :: Map.Map String Item
  , _objectStates :: ObjectStates
  }

-- Тип интерпретатора для AdventureL: стек из монад State и IO.
type Interpreter a = StateT Runtime IO a
```

Язык `AdventureL` - чистый монадический.
Интерпретатор этого языка - нечистый монадический (работает в монаде `IO`).

Механика игровых объектов
-------------------------

Игровые объекты - это такие объекты, у которых есть изменяемое состояние.
Оно может изменяться как в результате действий игрока,
так и по заранее заданным в сценариях причинам.

Состояние игрового объекта описывается в конкретной игре отдельным типом.
Этот тип неизвестен движку. Единственное на данный момент требование -
чтобы он был сериализуем.

Пример:

```haskell
-- Сериализуемый тип - состояние почтового ящика (но не сам объект)
data MailboxSt = MailboxSt
  { _description :: String      -- ^ Текстовое описание ящика
  , _container   :: Container   -- ^ "Контейнер", которым ящик и является
  }
  deriving (Generic, ToJSON, FromJSON)
```

Здесь ADT `MailboxSt` - это состояние почтового ящика.
Классы типов `Generic`, `ToJSON` и `FromJSON` делают его сериализуемым.

Поскольку ящик - это контейнер, то ему добавляется поле `_container`, имеющее специальный тип `Container`.
Контейнеры могут быть открытыми или закрытыми, а также могут содержать другие
объекты:

```haskell
data ContainerState = Opened | Closed
  deriving (Generic, ToJSON, FromJSON)

-- Тип, описывающий контейнер.
-- На данный момент содержит заглушку для хранимых в нем объектов - тип `Item`.
data Container = Container
  { _state :: ContainerState
  , _items :: [Item]
  }
  deriving (Generic, ToJSON, FromJSON)
```

Пример:

```haskell
mailboxObj = MailboxSt
  { _description = "This is a small mailbox."
  , _container = Container Closed ["leaflet"]
  }
```

Описанный выше тип `MailboxSt` - это только состояние объекта, а не сам объект.
Текущий подход требует, чтобы для каждого такого типа был еще указан "представительский" ADT,
позволяющий более удобно работать с объектами этого типа и генеричными операциями.

```haskell
data MailboxType = MailboxType
```

Также нужно еще описать операции, которые можно с выполнять над экземлпярами этого объекта.
Операции ( `Actions`). Например, почтовый ящик можно открыть и закрыть, потому что он
является контейнером. Типы для операций над объектами будут описаны позже.

Финальный генеричный игровой объект будет описываться сложным типом `Object`, причем
фантомным параметром будет передаваться "представительский" тип как `objType`:

```haskell
-- | Генеричный игровой объект.
data Object objType objSt = Object
  { _name    :: ObjectName      -- ^ Имя объекта (фактически, ключ в мапе всех игровых объектов).
  , _state   :: objSt           -- ^ Состояние объекта.
  , _actions :: Actions objSt   -- ^ Доступные действия с объектом.
  }
```

В случае с почтовым ящиком, `Object` будет параметризован следующими типами:

`objType :: MailboxType`
`objSt   :: MailboxSt`

Так как тип `Actions` на данный момент несериализуем (является функциями и сценариями в монаде `AdventureL`),
то чтобы привязать состояние объекта к действиям над ним, нельзя просто получить
`objSt` из состояния игры, нужно еще знать, какие `actions` привязаны к этому типу.
Чтобы это сделать, используется класс типов `ToObject` и "представительский" `objType`:

```haskell
class FromJSON objSt => ToObject objType objSt | objType -> objSt, objSt -> objType where
  object :: objSt -> Object objType objSt
```

Пример для почтового ящика:

```haskell
instance ToObject MailboxType MailboxSt where
  object objSt = Object "mailbox" objSt $ Map.fromList
    [ ("open",  Action openContainer  onMailboxOpenSuccess  onMailboxOpenFail  )
    , ("close", Action closeContainer onOpenCloseSuccess onMailboxCloseFail )
    ]
```

Здесь список содержит действия, выполняемые на ту или иную команду (`"open"`, `"close"`).
Действия содержат основную операцию (`openContainer`, `closeContainer`) и операции-хэндлеры,
исполняющиеся при успешной или неуспешной основной операции (`onMailboxOpenSuccess`, `onMailboxOpenFail`).

Наконец, для работы с генеричными объектами в сценариях имеются функции `getObject`, `getObject'` и `putObject`,
которые достают состояние объекта из состояния игры, десериализуют его
и позволяют работать с ним в сценарии; либо кладут состояние объекта назад
в состояние игры, предварительно засериализовав.
Функции используют класс типов `ToObject` и тип-представитель `objType`,
чтобы десериализовать объект до правильного типа-состояния `objSt`:

```haskell
getObject
  :: (FromJSON objSt, ToObject objType objSt)
  => ObjectName  -- ^ Имя объекта как ключ
  -> AdventureL (Object objType objSt)
getObject name = do
  objSt <- liftF $ GetObjSt name id
  pure $ object objSt

getObject'
  :: (FromJSON objSt, ToObject objType objSt)
  => objType     -- ^ "Представительский" тип
  -> ObjectName  -- ^ Имя объекта как ключ
  -> AdventureL (Object objType objSt)
getObject' _ = getObject

putObject
  :: ToJSON objSt
  => ObjectName   -- ^ Имя объекта как ключ
  -> objSt        -- ^ Состояние объекта
  -> AdventureL ()
putObject name objSt = liftF $ PutObjSt name objSt ()
```

Пример сценария, в котором запрашивается некоторый почтовый ящик, и рассказывается,
что в нем лежит:

```haskell
describeMailbox :: AdventureL ()
describeMailbox = do
  mailbox :: Mailbox <- getObject "mailbox"
  when showMailbox $ printMessage $ describeObject mailbox  -- Специальная операция, печатающая _description объекта.
```

В будущем планируется заменить представительский `objType` на строковые типы-литералы.

Actions
-------

TODO

Создание игры (локации, переходы)
---------------------------------

TODO
