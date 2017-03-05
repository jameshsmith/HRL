{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Component.Activator
    ( Activator
    , mkActivator
    , fixActivator
    , activate
    , door
    , openDoor
    ) where

import Prelude hiding ((.), id)

import Core.Types
import Core.Monad
import Core.Engine
import Core.ECS (Component, stock)

import Control.Monad.Reader
import Data.Typeable

newtype Activator =
    Activator (Fix (ReaderT (ARef, (Row, Col)) (Game () Level)))
    deriving (Typeable, Monoid)

instance Component Activator where
  stock = mempty

fixActivator :: Fix (ReaderT (ARef, (Row, Col)) (Game () Level)) -> Activator
fixActivator = Activator

mkActivator :: (s -> ARef -> (Row, Col) -> Game () Level s) -> s -> Activator
mkActivator f s = fixActivator (Ana (ReaderT . uncurry . f) s)

activate :: ARef -> (Row, Col) -> Game k Level ()
activate ref pos = noTurn $ do
    Activator f <- access (static pos)
    static pos ~= fmap Activator (runReaderT (step f) (ref, pos))
    return ()

data Door = Open | Closed

door :: Activator
door = mkActivator doorTrans Closed

openDoor :: Activator
openDoor = mkActivator doorTrans Open

doorTrans :: Door -> ARef -> (Row, Col) -> Game () Level Door

doorTrans Closed _ self = do
    staticChar self != '-'
    ix self <# solid != False
    ix self <# opacity != False
    modifyLevel shadowCast
    return Open

doorTrans Open _ self = do
    staticChar self != '+'
    ix self <# solid != True
    ix self <# opacity != True
    return Closed
