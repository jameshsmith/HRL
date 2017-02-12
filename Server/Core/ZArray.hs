module Core.ZArray
    ( ZArray (..)
    , unZArray
    ) where

import Control.Comonad
import Control.Monad
import Data.Array
import Data.Array.ST

data ZArray i e = ZArray i (Array i e)

unZArray :: ZArray i e -> Array i e
unZArray (ZArray _ arr) = arr

instance Ix i => Functor (ZArray i) where
  fmap f (ZArray i arr) = ZArray i (fmap f arr)

instance Ix i => Comonad (ZArray i) where
  extract (ZArray i arr) = arr ! i

  extend f (ZArray i arr) = ZArray i $ runSTArray $ do
    arr' <- newArray_ (bounds arr)
    forM_ (indices arr) $ \i' -> writeArray arr' i' (f (ZArray i' arr))
    return arr'
