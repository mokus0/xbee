{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.XBee.Series1.Frame.Tx where

import Network.Protocol.XBee.Series1.Addr
import Network.Protocol.XBee.Series1.Frame

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Bits
import Data.Serialize
import Data.Word

newtype TxOpts = TxOpts { getTxOpts :: Word8 }
    deriving (Eq, Ord, Bits, Read, Show)

txDisableAck, txPANIDBroadcast :: TxOpts
txDisableAck      = TxOpts 0x01
txPANIDBroadcast  = TxOpts 0x04

data Tx = Tx
    { txFrameID     :: !Word8
    , txOptions     :: !(Maybe TxOpts)
    , txDestAddr    :: !Addr
    , txMessage     :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame Tx where
    apiType tx
        | isFullAddr (txDestAddr tx)    = 0x00
        | otherwise                     = 0x01
    
    putBody (Tx frameId mbOpts addr msg) = do
        putWord8 frameId
        putAddr addr
        putWord8 (maybe 0 getTxOpts mbOpts)
        putByteString msg
    
    getBody t
        | t `notElem` [0x00, 0x01]  = Nothing
        | otherwise                 = Just $ do
            frameId <- getWord8
            addr    <- getAddr (t == 0x00)
            let txOpts 0 = Nothing
                txOpts x = Just (TxOpts x)
            opts    <- txOpts <$> getWord8
            msg     <- getBytes =<< remaining
            return (Tx frameId opts addr msg)

