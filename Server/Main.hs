{-# LANGUAGE ViewPatterns, MultiWayIf #-}
module Main where

import Prelude hiding ((.), id)

import Abyss.Game
import Abyss.Stats
import Abyss.Item
import Core.Types
import Core.Monad
import Core.Engine
import qualified Core.ECS as ECS
import Component.Modifier (selfModify)
import Server.Save

import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.State (runState)
import Data.Maybe (listToMaybe)
import System.Console.GetOpt
import System.Environment (getArgs)
import qualified System.Exit as Exit
import System.IO
import System.IO.Error
import System.Random (StdGen, getStdGen, randomR)
import System.CPUTime

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import qualified Data.Aeson as J
import qualified Data.Serialize as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendMany)

data Opts = Opts
    { optReplay :: Maybe Int
    }

defaultOpts :: Opts
defaultOpts = Opts Nothing

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option ['r'] ["replay"]
        (OptArg (\n opts -> opts { optReplay = join (fmap fst . listToMaybe . reads <$> n) }) "N")
        "Number of steps to replay from the save file. If omitted the entire save is replayed."
    ]

serverOpts :: [String] -> IO Opts
serverOpts argv =
    case getOpt Permute options argv of
        (o, _, []) -> return $ foldr id defaultOpts o
        (_, _, errs) -> do
            putStrLn $ concat errs
            putStrLn $ usageInfo "Invalid arguments" options
            Exit.exitFailure

main :: IO ()
main = withSocketsDo $ do
    opts <- serverOpts =<< getArgs

    -- Load the save file
    (sav, hdl) <- loadSave (optReplay opts) "save"

    -- Initialize socket
    addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serverAddr = head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    bind sock (addrAddress serverAddr)

    putStrLn "Waiting for client..."
    listen sock 1
    (conn, _) <- accept sock
    putStrLn "Connected!"
    
    evalGame hdl sav conn $ do

        closeSave "save" hdl
        close conn
        close sock
    
        Exit.exitSuccess

evalGame :: Handle
         -> [Input]
         -> Socket
         -> IO ()
         -> IO ()
evalGame hdl sav conn exitContinuation = do
    getCPUTime >>= (\t -> putStrLn ("starting at: " ++ show t))
    load sav initGame defaultLevel 1 0
  where
    load :: [Input] -> Game Action Level () -> Level -> Int -> Int -> IO ()
    load [] g l n _ = (getCPUTime >>= (\t -> putStrLn ("stopping at: " ++ show t))) >> sendJSON conn (RespondLoading 100) >> getStdGen >>= go g l n
    load (cmd : cmds) g l n m = do
        let (res, l') = (runState . runGame) g l
        when (m `mod` 1000 == 0) $ sendJSON conn (RespondLoading ((m * 100) `div` length sav))
        case res of
            Free (Turn cont)   | InputAct act <- cmd -> load cmds (cont act) l' n (m + 1)
            Free (YesNo cont)  | InputYN b <- cmd    -> load cmds (cont b) l' n (m + 1)
            Free (Unique cont)                       -> load (cmd : cmds) (cont n) l' (n + 1) (m + 1)
            Free (Dice _ cont) | InputRoll r <- cmd  -> load cmds (cont r) l' n (m + 1)
            Free (Gen _ cont)  | InputLev g <- cmd   -> load cmds (cont g) l' n (m + 1)
            Pure () -> hClose hdl >> error "Unexpected return from game!"
            _       -> hClose hdl >> error "Invalid save file"

    go :: Game Action Level () -> Level -> Int -> StdGen -> IO ()
    go g l n gen = do
        let (res, l') = runGameState g l
        case res of
            Free (Turn cont) -> do
                sendJSON conn (RespondLevel l')
                message <- talk conn l'
                case message of
                    Nothing -> exitContinuation
                    Just act -> do
                        BS.hPut hdl (S.encode (InputAct act))
                        go (cont act) (clearMessages l') n gen

            Free (YesNo cont) -> do
                BS.hPut hdl (S.encode (InputYN True))
                go (cont True) l' n gen
          
            Free (Unique cont) -> go (cont n) l' (n + 1) gen
          
            Free (Dice (lo, hi) cont) -> do
                let (r, gen') = randomR (lo, hi) gen
                BS.hPut hdl (S.encode (InputRoll r))
                go (cont r) l' n gen'

            Free (Gen generator cont) -> do
                let (lev, gen') = runState generator gen
                BS.hPut hdl (S.encode (InputLev lev))
                go (cont lev) l' n gen'
                
            Pure () -> return ()

talk :: Socket -> Level -> IO (Maybe Action)
talk conn lev = do
    message <- recvJSON conn
    case message of
        Nothing -> return Nothing
        Just (RequestInventory l) -> sendJSON conn (RespondInventory l lev) >> talk conn lev
        Just (RequestAct act) -> return (Just act)

{- ============================================================
   Protocol for sending and receiving JSON data with the client
   ============================================================ -}

-- | Send JSON on a socket.
sendJSON :: J.ToJSON a => Socket -> a -> IO ()
sendJSON sock x = sendMany sock (prefix (J.encode x) : LBS.toChunks (J.encode x))
  where
    prefix = BS.pack . map (fromIntegral . fromEnum) . (++ "#") . show . LBS.length

-- | Receive JSON from a socket. Returns @Nothing@ if there are any
-- kind of parsing errors, or errors with the socket.
recvJSON :: J.FromJSON a => Socket -> IO (Maybe a)
recvJSON sock = recvJSON' sock `catchIOError` const (return Nothing)

-- | Read an Int from an ASCII bytestring
readInt :: BS.ByteString -> Int
readInt = BS.foldl' (\x y -> x * 10 + (fromIntegral (y - 48))) 0

recvJSON' :: J.FromJSON a => Socket -> IO (Maybe a)
recvJSON' sock = do
    chunk <- recv sock 4096
    BS.putStrLn chunk
    let (readInt -> prefix, json) = BS.break (== 35) chunk  -- 35 is Ascii '#'
    -- BS.putStrLn (BS.tail json)
    -- putStrLn (show prefix)
    if | BS.null json -> return Nothing
       | BS.length json > prefix -> return $ J.decodeStrict (BS.tail json)
       | otherwise -> do
           chunks <- recvUntil sock (prefix - BS.length json - 1)
           return $ J.decode (LBS.fromChunks (chunk : chunks))

recvUntil :: Socket -> Int -> IO [BS.ByteString]
recvUntil sock n
  | n <= 4096 = pure <$> recv sock n
  | otherwise = (:) <$> recv sock 4096 <*> recvUntil sock (n - 4096)

-- | Data we send to the client.
data Response = RespondLevel Level
              | RespondLoading Int
              | RespondInventory (Row, Col) Level

responseObject :: (a -> J.Value) -> String -> a -> J.Value 
responseObject encoder msgType payload = J.object
    [ (T.pack "type", J.toJSON msgType)
    , (T.pack "payload", encoder payload)
    ]

entityHpJSON :: ECS.Entity -> J.Value
entityHpJSON (selfModify -> ent) = J.object
    [ (T.pack "hurt", J.toJSON (hurt (getL ECS.lens ent)))
    , (T.pack "hp", J.toJSON (getL (baseHP . ECS.lens) ent))
    ]

instance J.ToJSON Response where
  toJSON (RespondLevel lev) = responseObject (levelToJSON entityHpJSON) "level" lev
  toJSON (RespondLoading n) = responseObject J.toJSON "loading" n
  toJSON (RespondInventory l lev) = responseObject id "inventory" (inventoryJSON l lev)

-- | Data we receive from the client.
data Request = RequestAct Action
             | RequestInventory (Row, Col)
             deriving Show

instance J.FromJSON Request where
  parseJSON = J.withObject "message" $ \obj -> do
      typ <- obj J..: T.pack "type"
      payload <- obj J..: T.pack "payload"
      if | typ == "action" -> RequestAct <$> J.parseJSON payload
         | otherwise -> mzero
