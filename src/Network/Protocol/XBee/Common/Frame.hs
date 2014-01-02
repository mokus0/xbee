module Network.Protocol.XBee.Common.Frame where

import Network.Protocol.XBee.Common.Envelope

import Data.Serialize
import Data.Traversable (sequenceA)
import Data.Word

class Frame f where
    putFrame :: Bool -> f -> Put
    getFrame :: Bool -> Get f

-- TODO: rename this, maybe "FrameContents"
class ApiFrame t where
    apiType :: t -> Word8
    putBody :: t -> Put
    getBody :: Word8 -> Maybe (Get t) -- TODO: use Alternative, or explain why not...

putApiFrame :: ApiFrame t => Bool -> t -> Put
putApiFrame esc frame =
    putEnvelope esc (apiType frame) (runPut (putBody frame))

getApiFrame :: ApiFrame t => Bool -> Get (Maybe t)
getApiFrame esc = do
    (t, bs) <- getEnvelope esc
    either fail return (runGet (sequenceA (getBody t)) bs)
