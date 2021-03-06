module Network.Protocol.XBee.Series1
    ( Addr(..)
    , Series1(..)
    
    , module Network.Protocol.XBee.Common.Frame
    , module Network.Protocol.XBee.Series1.Frame.ATCmd
    , module Network.Protocol.XBee.Series1.Frame.ATResponse
    , module Network.Protocol.XBee.Series1.Frame.ModemStatus
    , module Network.Protocol.XBee.Series1.Frame.Rx
    , module Network.Protocol.XBee.Series1.Frame.Tx
    , module Network.Protocol.XBee.Series1.Frame.TxStat
    ) where

import Network.Protocol.XBee.Common.Envelope
import Network.Protocol.XBee.Common.Frame
import Network.Protocol.XBee.Series1.Addr
import Network.Protocol.XBee.Series1.Frame.ATCmd
import Network.Protocol.XBee.Series1.Frame.ATResponse
import Network.Protocol.XBee.Series1.Frame.ModemStatus
import Network.Protocol.XBee.Series1.Frame.Rx
import Network.Protocol.XBee.Series1.Frame.Tx
import Network.Protocol.XBee.Series1.Frame.TxStat

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word

data Series1
    = ATCmdFrame        !ATCmd
    | ATResponseFrame   !ATResponse
    | ModemStatusFrame  !ModemStatus
    | RxFrame           !Rx
    | TxFrame           !Tx
    | TxStatFrame       !TxStat
    | UnknownFrame      !Word8 !BS.ByteString
    deriving (Eq, Ord, Read, Show)

instance Frame Series1 where
    putFrame esc (ATCmdFrame         f) = putApiFrame esc f
    putFrame esc (ATResponseFrame    f) = putApiFrame esc f
    putFrame esc (ModemStatusFrame   f) = putApiFrame esc f
    putFrame esc (RxFrame            f) = putApiFrame esc f
    putFrame esc (TxFrame            f) = putApiFrame esc f
    putFrame esc (TxStatFrame        f) = putApiFrame esc f
    putFrame esc (UnknownFrame  t rest) = putEnvelope esc t rest

    getFrame esc = do
        (t, rest) <- getEnvelope esc
        
        let mbGet = msum
                [ fmap ATCmdFrame       <$> getBody t
                , fmap ATResponseFrame  <$> getBody t
                , fmap ModemStatusFrame <$> getBody t
                , fmap RxFrame          <$> getBody t
                , fmap TxFrame          <$> getBody t
                , fmap TxStatFrame      <$> getBody t
                ]
        
        case mbGet of
            Nothing     -> return $! UnknownFrame t rest
            Just getIt  -> either fail return (runGet getIt rest)


instance Serialize Series1 where
    put = putFrame True
    get = getFrame True