{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.XBee.Series2.Frame.Tx where

import Network.Protocol.XBee.Common.Frame

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Bits
import Data.Serialize
import Data.Word

newtype TxOpts = TxOpts { getTxOpts :: Word8 }
    deriving (Eq, Ord, Bits, Read, Show)

txDisableRetries, txEnableAPSEncryption, txUseExtendedTxTimeout :: TxOpts
txDisableRetries        = TxOpts 0x01
txEnableAPSEncryption   = TxOpts 0x20
txUseExtendedTxTimeout  = TxOpts 0x40

data Tx = Tx
    { txFrameID             :: !Word8
    , txDestAddr64          :: !Word64
    , txDestAddr16          :: !Word16
    , txBroadcastRadius     :: !Word8
    , txOptions             :: !(Maybe TxOpts)
    , txMessage             :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame Tx where
    apiType _ = 0x10
    
    putBody (Tx frameId addr64 addr16 radius mbOpts msg) = do
        putWord8 frameId
        putWord64be addr64
        putWord16be addr16
        putWord8 radius
        putWord8 (maybe 0 getTxOpts mbOpts)
        putByteString msg
    
    getBody 0x10 = Just $ do
        frameId <- getWord8
        addr64  <- getWord64be
        addr16  <- getWord16be
        radius  <- getWord8
        let txOpts 0 = Nothing
            txOpts x = Just (TxOpts x)
        mbOpts  <- txOpts <$> getWord8
        msg     <- getBytes =<< remaining
        return (Tx frameId addr64 addr16 radius mbOpts msg)
    getBody _ = Nothing
