module Network.Protocol.XBee.Series1.Frame.ATResponse where

import Network.Protocol.XBee.Series1.Addr
import Network.Protocol.XBee.Common.Frame

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word

newtype ATResponseStatus = ATResponseStatus Word8
    deriving (Eq, Ord, Read, Show)

atRspOK, atRspError, atRspInvalidCmd, atRspInvalidParam :: ATResponseStatus
atRspOK             = ATResponseStatus 0
atRspError          = ATResponseStatus 1
atRspInvalidCmd     = ATResponseStatus 2
atRspInvalidParam   = ATResponseStatus 3

data ATResponse = ATResponse
    { atRspFrameID      :: !Word8
    , atRspRemoteAddr   :: !(Maybe Addr)
    , atRspCommand      :: !Word16
    , atRspStatus       :: !ATResponseStatus
    , atRspValue        :: !BS.ByteString
    } deriving (Eq, Ord, Read, Show)

instance ApiFrame ATResponse where
    apiType ATResponse {atRspRemoteAddr = Nothing}  = 0x88
    apiType _                                       = 0x97
    
    putBody (ATResponse frameID Nothing cmd (ATResponseStatus stat) val) = do
        putWord8        frameID
        putWord16be     cmd
        putWord8        stat
        putByteString   val
    putBody (ATResponse frameID (Just addr) cmd (ATResponseStatus stat) val) = do
        putWord8 frameID
        
        let (addr16, addr64) = case addr of
                Addr16 x -> (x,      0)
                Addr64 x -> (0xFFFE, x)
        putWord64be addr64
        putWord16be addr16
        
        putWord16be     cmd
        putWord8        stat
        putByteString   val
    
    getBody 0x88 = Just $ do
        frameID <- getWord8
        cmd     <- getWord16be
        stat    <- ATResponseStatus <$> getWord8
        val     <- getBytes =<< remaining
        
        return (ATResponse frameID Nothing cmd stat val)
    getBody 0x97 = Just $ do
        frameID <- getWord8
        
        addr64  <- getWord64be
        addr16  <- getWord16be
        let addr = if addr16 == 0xFFFE
                then Addr64 addr64
                else Addr16 addr16
        
        cmd     <- getWord16be
        stat    <- ATResponseStatus <$> getWord8
        val     <- getBytes =<< remaining
        
        return (ATResponse frameID (Just addr) cmd stat val)
    getBody _ = Nothing
