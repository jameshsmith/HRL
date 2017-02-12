{-# LANGUAGE LambdaCase #-}
module Server.Save (loadSave, closeSave, Input (..)) where

import Prelude hiding ((.), id)

import Core.Types

import Abyss.Game

import Control.Applicative
import Data.Array.Unboxed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as S
import System.FilePath
import System.IO
import System.Directory

import qualified Codec.Compression.Zlib as Zlib

loadSave :: Maybe Int -> FilePath -> IO ([Input], Handle)
loadSave replay path = do
    doesFileExist path >>= \case
      True -> return ()
      False -> BS.writeFile path (compress BS.empty)
    Right sav <- readSave <$> BS.readFile path
    let sav' = maybe sav (\n -> take n sav) replay
    hdl <- openFile (path <.> "log") WriteMode
    mapM_ (BS.hPut hdl . S.encode) sav'
    hFlush hdl
    return (sav', hdl)

closeSave :: FilePath -> Handle -> IO ()
closeSave path hdl = do
    hClose hdl
    bs <- BS.readFile (path <.> "log")
    BS.writeFile path (compress bs)
    removeFile (path <.> "log")

data Input = InputAct Action
           | InputRoll Int
           | InputYN Bool
           | InputLev (UArray (Row, Col) Char)
           deriving Show

instance S.Serialize Input where
  put (InputAct a)  = S.putWord8 0x00 >> S.put a
  put (InputRoll r) = S.putWord8 0x01 >> S.put r
  put (InputYN b)   = S.putWord8 0x02 >> S.put b
  put (InputLev g)  = S.putWord8 0x03 >> S.put g

  get = (S.getWord8) >>= \case
    0x00 -> InputAct <$> S.get
    0x01 -> InputRoll <$> S.get
    0x02 -> InputYN <$> S.get
    0x03 -> InputLev <$> S.get
    _ -> error "Invalid input"

getInputs :: S.Get [Input]
getInputs = ((:) <$> S.get <*> getInputs) <|> pure []
    
compress :: BS.ByteString -> BS.ByteString
compress = LBS.toStrict . Zlib.compress . LBS.fromStrict

decompress :: BS.ByteString -> BS.ByteString
decompress = LBS.toStrict . Zlib.decompress . LBS.fromStrict

readSave :: BS.ByteString -> Either String [Input]
readSave bs = S.runGet getInputs (decompress bs)
