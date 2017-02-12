module Core.SerialUtil where

import Control.Monad
import Data.Serialize

liftGet2 f = liftM2 f get get 

liftGet3 f = liftM3 f get get get

liftGet4 f = liftM4 f get get get get

liftGet5 f = liftM5 f get get get get get

liftGet6 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6}

liftGet7 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7}

liftGet8 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8}

liftGet9 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; pure $ return (f x1 x2 x3 x4 x5 x6 x7 x8 x9)}

liftGet10 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10}

liftGet11 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get; x11 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11}

liftGet12 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get; x11 <- get; x12 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12}

liftGet13 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get; x11 <- get; x12 <- get; x13 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13}

liftGet14 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get; x11 <- get; x12 <- get; x13 <- get; x14 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14}

liftGet15 f = do
  { x1 <- get; x2 <- get; x3 <- get; x4 <- get; x5 <- get; x6 <- get; x7 <- get; x8 <- get; x9 <- get
  ; x10 <-get; x11 <- get; x12 <- get; x13 <- get; x14 <- get; x15 <- get
  ; pure $ f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15}
