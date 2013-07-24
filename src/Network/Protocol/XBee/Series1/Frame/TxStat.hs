module Network.Protocol.XBee.Series1.Frame.TxStat where

import Network.Protocol.XBee.Series1.Frame

import Control.Applicative
import Data.Serialize
import Data.Word

newtype TxStatus = TxStatus { getTxStatus :: Word8}
    deriving (Eq, Ord, Read, Show)

txSuccess, txNoAck, txCCAFailed, txPurged :: TxStatus
txSuccess   = TxStatus 0
txNoAck     = TxStatus 1
txCCAFailed = TxStatus 2
txPurged    = TxStatus 3

data TxStat = TxStat
    { txStatFrameID :: !Word8
    , txStatus      :: !TxStatus
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame TxStat where
    apiType _ = 0x89
    
    putBody (TxStat frameId status) = do
        putWord8 frameId
        putWord8 (getTxStatus status)
    
    getBody 0x89 = Just (TxStat <$> getWord8 <*> (TxStatus <$> getWord8))
    getBody _    = Nothing
