{-# LANGUAGE RecordWildCards #-}
module Network.Protocol.XBee
    ( XBee
    , runXBee
    , readXBee
    , writeXBee
    , closeXBee
    ) where

import Network.Protocol.XBee.Series1

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E (SomeException, catch, throwIO)
import Control.Monad.Fix
import qualified Data.ByteString as BS
import Data.Serialize
import System.IO

data TxControlMsg
    = Send !Frame
    | EndTx

data RxControlMsg
    = Rcv       !Frame
    | FrameErr  !String
    | Exc       !SomeException
    | EndRx

data XBee = XBee
    { handle        :: !Handle
    
    , rxChan        :: !(TQueue RxControlMsg)
    , rxThreadId    :: !ThreadId
    
    , txChan        :: !(TQueue TxControlMsg)
    , txThreadId    :: !ThreadId
    }

runXBee :: Bool -> Handle -> IO XBee
runXBee esc h = do
    hSetBuffering h NoBuffering
    mfix $ \xbee -> XBee h <$> newTQueueIO
       <*> forkIO (rxThread esc xbee)
       <*> newTQueueIO
       <*> forkIO (txThread esc xbee)

-- TODO: fix this interface, it's ugly (too much 'fail', no nonblocking read, etc)
readXBee :: XBee -> IO Frame
readXBee XBee{..} = do
    msg <- atomically $ do
        msg <- readTQueue rxChan
        case msg of
            EndRx -> unGetTQueue rxChan EndRx
            _     -> return ()
        return msg
    
    case msg of
        Rcv      m  -> return m
        FrameErr e  -> fail e
        Exc      e  -> throwIO e
        EndRx       -> fail "readXBee: connection is closed"

writeXBee :: XBee -> Frame -> IO ()
writeXBee x = atomically . writeTQueue (txChan x) . Send

closeXBee :: XBee -> IO ()
closeXBee XBee{..} = atomically (writeTQueue txChan EndTx)

rxThread :: Bool -> XBee -> IO ()
rxThread esc XBee{..} = go start
    where
        nTries  = 3 :: Int
        next    = next' nTries
        
        next' 0 = return Nothing
        next' n = do
            (Just <$> BS.hGetSome handle 512) `E.catch` \e -> do
                end <- hIsEOF handle
                if end
                    then next' 0
                    else do
                        atomically (writeTQueue rxChan (Exc e))
                        next' (n-1)
        
        start           = restart BS.empty
        restart         = runGetPartial (getFrame esc)
        
        go (Fail msg)   = atomically (writeTQueue rxChan (FrameErr msg)) >> go start -- TODO: resync
        go (Partial k)  = maybe (atomically (writeTQueue rxChan EndRx)) (go . k) =<< next 
        go (Done f bs)  = atomically (writeTQueue rxChan (Rcv f)) >> go (restart bs)
        
txThread :: Bool -> XBee -> IO ()
txThread esc XBee{..} = go =<< next
    where
        next = atomically (readTQueue txChan)
        
        go EndTx = do
            -- TODO: this doesn't appear to interrupt the blocking hGetSome...
            -- make sure the rx thread gets the message
            hClose handle 
        go (Send msg) = do
            BS.hPutStr handle (runPut (putFrame esc msg))
            hFlush handle
            go =<< next
        