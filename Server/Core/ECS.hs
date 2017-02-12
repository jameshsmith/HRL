{-# LANGUAGE TypeOperators, ExistentialQuantification #-}
module Core.ECS
    ( Entity, entity
    , Component, stock, component
    , HasEntity
    , empty
    , state
    , insert
    , modify
    , singleton
    , lens
    ) where

import Prelude hiding ((.), id)

import Core.Types ((:->))

import Control.Category
import Data.Dynamic
import qualified Data.Lens.Common as Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

class Typeable s => Component s where
  stock :: s

data Container = forall s. Component s => Container s

newtype Entity = Entity (Map TypeRep Container)

empty :: Entity
empty = Entity Map.empty

state' :: Component s => s -> Entity -> s
state' inhab (Entity cs)
  | Just (Container s) <- Map.lookup (typeOf inhab) cs = fromJust (cast s)
  | otherwise = stock

state :: Component s => Entity -> s
state = state' stock
    
insert :: Component s => s -> Entity -> Entity
insert v (Entity cs) = Entity $ Map.insert (typeOf v) (Container v) cs

modifyC :: Component s => (s -> s) -> Container -> Container
modifyC f (Container s)
  | Just s' <- cast s = Container (f s')
  | otherwise = Container s

modify' :: Component s => s -> (s -> s) -> Entity -> Entity
modify' inhab f (Entity cs) = Entity $ Map.alter f' (typeOf inhab) cs
  where
    f' (Just c) = Just (modifyC f c)
    f' Nothing = Just (Container (f stock))
  
modify :: Component s => (s -> s) -> Entity -> Entity
modify = modify' stock

singleton :: Component s => s -> Entity
singleton v = insert v empty

lens :: Component s => Entity :-> s
lens = Lens.lens state insert

class HasEntity e where
    entity :: e :-> Entity

instance HasEntity Entity where
    entity = id

component :: (Component s, HasEntity e) => e :-> s
component = lens . entity
