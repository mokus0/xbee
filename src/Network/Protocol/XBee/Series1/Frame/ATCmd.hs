module Network.Protocol.XBee.Series1.Frame.ATCmd where

import Network.Protocol.XBee.Series1.Addr
import Network.Protocol.XBee.Series1.Frame

import Data.Bits
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Word

data ATCmd = ATCmd
    { atFrameID     :: !Word8
    , atImmediate   :: !Bool
    , atRemoteAddr  :: !(Maybe Addr)
    , atCommand     :: !Word16
    , atParamValue  :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame ATCmd where
    apiType ATCmd { atRemoteAddr = Just{} } = 0x17
    apiType ATCmd { atImmediate  = True   } = 0x08
    apiType _                               = 0x09
    
    putBody (ATCmd frameID _ Nothing cmd val) = do
        putWord8 frameID
        putWord16be cmd
        putByteString val
    putBody (ATCmd frameID imm (Just addr) cmd val) = do
        putWord8 frameID
        
        let (addr16, addr64) = case addr of
                Addr16 x -> (x,      0)
                Addr64 x -> (0xFFFE, x)
        putWord64be addr64
        putWord16be addr16
        
        putWord8 (if imm then bit 2 else 0)
        putWord16be cmd
        putByteString val
    
    getBody t
        | t `elem` [0x08, 0x09] = Just $ do
            frameID <- getWord8
            cmd     <- getWord16be
            val     <- getBytes =<< remaining
            return (ATCmd frameID (t == 0x08) Nothing cmd val)
        
        | t == 0x17             = Just $ do
            frameID <- getWord8
            
            addr64  <- getWord64be
            addr16  <- getWord16be
            let addr = if addr16 == 0xFFFE
                    then Addr64 addr64
                    else Addr16 addr16
            
            opts    <- getWord8
            cmd     <- getWord16be
            val     <- getBytes =<< remaining
            
            return (ATCmd frameID (testBit opts 2) (Just addr) cmd val)
        
        | otherwise             = Nothing
