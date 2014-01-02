module Network.Protocol.XBee.Series1.Frame.ModemStatus where

import Network.Protocol.XBee.Common.Frame

import Control.Applicative
import Data.Serialize
import Data.Word

newtype ModemStatus = ModemStatus Word8
    deriving (Eq, Ord, Read, Show)

hardwareReset :: ModemStatus
hardwareReset = ModemStatus 0

watchdogReset :: ModemStatus
watchdogReset = ModemStatus 1

associated :: ModemStatus
associated = ModemStatus 2

disassociated :: ModemStatus
disassociated = ModemStatus 3

syncLost :: ModemStatus
syncLost = ModemStatus 4

coordinatorRealignment :: ModemStatus
coordinatorRealignment = ModemStatus 5

coordinatorStarted :: ModemStatus
coordinatorStarted = ModemStatus 6

instance ApiFrame ModemStatus where
    apiType _ = 0x8a
    
    putBody (ModemStatus x) = putWord8 x
    getBody 0x8a = Just (ModemStatus <$> getWord8)
    getBody _    = Nothing
