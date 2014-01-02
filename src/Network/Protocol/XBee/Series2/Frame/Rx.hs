{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.XBee.Series2.Frame.Rx where

import Network.Protocol.XBee.Common.Frame

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Bits
import Data.Serialize
import Data.Word

newtype RxOpts = RxOpts {getRxOpts :: Word8}
    deriving (Eq, Ord, Read, Show, Bits)

rxPacketAcknowledged, rxAddrBroadcast, rxAPSEncrypted, rxSentFromEndDevice :: RxOpts
rxPacketAcknowledged    = RxOpts 0x01
rxAddrBroadcast         = RxOpts 0x02
rxAPSEncrypted          = RxOpts 0x20
rxSentFromEndDevice     = RxOpts 0x40

data Rx = Rx
    { rxSrcAddr64   :: !Word64
    , rxSrcAddr16   :: !Word16
    , rxOptions     :: !(Maybe RxOpts)
    , rxMessage     :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame Rx where
    apiType _ = 0x90
    
    putBody (Rx addr64 addr16 mbOpts msg) = do
        putWord64be addr64
        putWord16be addr16
        putWord8 (maybe 0 getRxOpts mbOpts)
        putByteString msg
    
    getBody 0x90 = Just $ do
        addr64 <- getWord64be
        addr16 <- getWord16be
        let rxOpts 0 = Nothing
            rxOpts x = Just (RxOpts x)
        opts <- rxOpts <$> getWord8
        msg  <- getBytes =<< remaining
        return (Rx addr64 addr16 opts msg)
    getBody _ = Nothing
