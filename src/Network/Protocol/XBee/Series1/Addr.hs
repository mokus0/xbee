module Network.Protocol.XBee.Series1.Addr where

import Control.Applicative
import Data.Serialize
import Data.Word

data Addr
    = Addr16 Word16
    | Addr64 Word64
    deriving (Eq, Ord, Read, Show)

isFullAddr :: Addr -> Bool
isFullAddr Addr16{} = False
isFullAddr Addr64{} = True

putAddr :: Addr -> Put
putAddr (Addr16 x) = putWord16be x
putAddr (Addr64 x) = putWord64be x

getAddr :: Bool -> Get Addr
getAddr True    = Addr64 <$> getWord64be
getAddr False   = Addr16 <$> getWord16be
