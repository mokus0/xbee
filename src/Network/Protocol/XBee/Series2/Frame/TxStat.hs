module Network.Protocol.XBee.Series2.Frame.TxStat where

import Network.Protocol.XBee.Common.Frame

import Control.Applicative
import Data.Serialize
import Data.Word

newtype TxDeliveryStatus = TxDeliveryStatus { getTxDeliveryStatus :: Word8}
    deriving (Eq, Ord, Read, Show)

newtype TxDiscoveryStatus = TxDiscoveryStatus { getTxDiscoveryStatus :: Word8}
    deriving (Eq, Ord, Read, Show)

-- TODO: all enum values
txSuccess, txNoMACAck, txCCAFailed :: TxDeliveryStatus
txSuccess   = TxDeliveryStatus 0x00
txNoMACAck  = TxDeliveryStatus 0x01
txCCAFailed = TxDeliveryStatus 0x02

data TxStat = TxStat
    { txStatFrameID     :: !Word8
    , txStatAddr16      :: !Word16
    , txStatRetryCount  :: !Word8
    , txDeliveryStatus  :: !TxDeliveryStatus
    , txDiscoveryStatus :: !TxDiscoveryStatus
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame TxStat where
    apiType _ = 0x8B
    
    putBody (TxStat frameId addr16 retries delivery discovery) = do
        putWord8    frameId
        putWord16be addr16
        putWord8    retries
        putWord8    (getTxDeliveryStatus delivery)
        putWord8    (getTxDiscoveryStatus discovery)
    
    getBody 0x8B = Just $ do
        frameId     <- getWord8
        addr16      <- getWord16be
        retries     <- getWord8
        delivery    <- TxDeliveryStatus  <$> getWord8
        discovery   <- TxDiscoveryStatus <$> getWord8
        return (TxStat frameId addr16 retries delivery discovery)
    getBody _    = Nothing
