module Network.Protocol.XBee.Common.Escape
    ( putEscaped
    , getEscaped
    ) where

import Control.Applicative
import Data.Bits
import Data.Serialize
import Data.Word

esc :: [Word8]
esc = [0x11, 0x13, 0x7d, 0x7e]

putEscaped :: Word8 -> Put
putEscaped b
    | b `elem` esc  = do
        putWord8 0x7d
        putWord8 (xor 0x20 b)
    | otherwise     = putWord8 b

getEscaped :: Get Word8
getEscaped = do
    b <- getWord8
    if b == 0x7d
        then xor 0x20 <$> getWord8
        else return b

