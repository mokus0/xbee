module Network.Protocol.XBee.Common.Envelope
    ( putEnvelope
    , getEnvelope
    ) where

import Network.Protocol.XBee.Common.Escape

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word
import Text.Printf

cksum :: Word8 -> BS.ByteString -> Word8
cksum b bs = BS.foldr subtract (0xFF - b) bs

putWord8e :: Bool -> Word8 -> Put
putWord8e True  = putEscaped
putWord8e False = putWord8

putEnvelope :: Bool -> Word8 -> BS.ByteString -> Put
putEnvelope esc apiType payload = do
    putWord8 0x7e
    let sz = 1 + BS.length payload
    putWord8e esc (fromIntegral (sz `shiftR` 8))
    putWord8e esc (fromIntegral sz)
    
    putWord8e esc apiType
    BS.foldr ((>>) . putWord8e esc) (return ()) payload
    
    putWord8e esc (cksum apiType payload)

getWord8e :: Bool -> Get Word8
getWord8e True  = getEscaped
getWord8e False = getWord8

getEnvelope :: Bool -> Get (Word8, BS.ByteString)
getEnvelope esc = do
    b <- getWord8
    when (b /= 0x7e) $ do
        fail (printf "Expected start character 0x7e, got 0x%02x" b)
    
    sz_hi <- getWord8e esc
    sz_lo <- getWord8e esc
    let sz = (fromIntegral sz_hi `shiftL` 8) .|. fromIntegral sz_lo
    
    when (sz == 0) $ fail "zero-length message"
    apiType <- getWord8e esc
    rest    <- BS.pack <$> replicateM (sz - 1) (getWord8e esc)
    
    cs      <- getWord8
    let     expected = cksum apiType rest
    when (cs /= expected) $ do
        fail (printf "Checksum failure: expected 0x%02x, got 0x%02x" cs expected)
    
    return (apiType, rest)
