module Network.Protocol.XBee.Series1.Frame where

import Network.Protocol.XBee.Series1.Envelope

import Data.Serialize
import Data.Traversable (sequenceA)
import Data.Word

class ApiFrame t where
    apiType :: t -> Word8
    putBody :: t -> Put
    getBody :: Word8 -> Maybe (Get t)

putApiFrame :: ApiFrame t => Bool -> t -> Put
putApiFrame esc frame =
    putEnvelope esc (apiType frame) (runPut (putBody frame))

getApiFrame :: ApiFrame t => Bool -> Get (Maybe t)
getApiFrame esc = do
    (t, bs) <- getEnvelope esc
    either fail return (runGet (sequenceA (getBody t)) bs)
