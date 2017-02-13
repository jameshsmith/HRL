module Abyss.Item
    ( Item
    , itemName
    , itemDesc
    , itemModifier
    , unsafeMkItem
    ) where

import qualified Core.ECS as ECS

import Data.Text (Text)

data Item = Item
    { _name     :: Text
    , _desc     :: Text
    , _modifier :: ECS.Entity -> ECS.Entity
    }

itemName :: Item -> Text
itemName = _name

itemDesc :: Item -> Text
itemDesc = _desc

itemModifier :: Item -> ECS.Entity -> ECS.Entity
itemModifier = _modifier

-- | In general, we can't just construct arbitrary items while the
-- game is running, due to serialisation issues, so this function is
-- marked unsafe.
unsafeMkItem :: Text -> Text -> (ECS.Entity -> ECS.Entity) -> Item
unsafeMkItem = Item
