{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.XBee.Series1.Frame.Rx where

import Network.Protocol.XBee.Series1.Addr
import Network.Protocol.XBee.Series1.Frame

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Bits
import Data.Serialize
import Data.Word

newtype RxOpts = RxOpts {getRxOpts :: Word8}
    deriving (Eq, Ord, Read, Show, Bits)

rxAddrBroadcast, rxPANIDBroadcast :: RxOpts
rxAddrBroadcast   = bit 1
rxPANIDBroadcast  = bit 2

data Rx = Rx
    { rxSrcAddr     :: !Addr
    , rxRSSI        :: !Word8
    , rxOptions     :: !(Maybe RxOpts)
    , rxMessage     :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame Rx where
    apiType rx
        | isFullAddr (rxSrcAddr rx) = 0x80
        | otherwise                 = 0x81
    
    putBody (Rx addr rssi mbOpts msg) = do
        putAddr addr
        putWord8 rssi
        putWord8 (maybe 0 getRxOpts mbOpts)
        putByteString msg
    
    getBody t
        | t `notElem` [0x80, 0x81]  = Nothing
        | otherwise                 = Just $ do
            addr <- getAddr (t == 0x80)
            rssi <- getWord8
            let rxOpts 0 = Nothing
                rxOpts x = Just (RxOpts x)
            opts <- rxOpts <$> getWord8
            msg  <- getBytes =<< remaining
            return (Rx addr rssi opts msg)
