module Network.Protocol.XBee.Series2
    ( Series2(..)
    
    , module Network.Protocol.XBee.Common.Frame
    , module Network.Protocol.XBee.Series2.Frame.Rx
    , module Network.Protocol.XBee.Series2.Frame.Tx
    , module Network.Protocol.XBee.Series2.Frame.TxStat
    ) where

import Network.Protocol.XBee.Common.Envelope
import Network.Protocol.XBee.Common.Frame
import Network.Protocol.XBee.Series2.Frame.Rx
import Network.Protocol.XBee.Series2.Frame.Tx
import Network.Protocol.XBee.Series2.Frame.TxStat

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word

data Series2
    = RxFrame           !Rx
    | TxFrame           !Tx
    | TxStatFrame       !TxStat
    | UnknownFrame      !Word8 !BS.ByteString
    deriving (Eq, Ord, Read, Show)

instance Frame Series2 where
    putFrame esc (RxFrame            f) = putApiFrame esc f
    putFrame esc (TxFrame            f) = putApiFrame esc f
    putFrame esc (TxStatFrame        f) = putApiFrame esc f
    putFrame esc (UnknownFrame  t rest) = putEnvelope esc t rest
    
    getFrame esc = do
        (t, rest) <- getEnvelope esc
        
        let mbGet = msum
                [ fmap RxFrame          <$> getBody t
                , fmap TxFrame          <$> getBody t
                , fmap TxStatFrame      <$> getBody t
                ]
        
        case mbGet of
            Nothing     -> return $! UnknownFrame t rest
            Just getIt  -> either fail return (runGet getIt rest)


instance Serialize Series2 where
    put = putFrame True
    get = getFrame True