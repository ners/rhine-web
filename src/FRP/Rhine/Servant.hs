module FRP.Rhine.Servant where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor (void)
import Data.Proxy (Proxy (Proxy))
import Data.Time (getCurrentTime)
import FRP.Rhine
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API
import Servant.Server
import Prelude

type RhineApi st = Get '[JSON] st :<|> ReqBody '[JSON] st :> Put '[JSON] NoContent

rhineApi :: Proxy (RhineApi st)
rhineApi = Proxy

newtype RequestClock st = RequestClock {port :: Port}

data Request st
    = Get (TMVar st)
    | Put st

instance (MonadIO m, FromJSON st, ToJSON st) => Clock m (RequestClock st) where
    type Time (RequestClock st) = UTCTime
    type Tag (RequestClock st) = Request st
    initClock RequestClock{..} = liftIO do
        requests <- newTQueueIO
        let server :: Server (RhineApi st)
            server = handleGet :<|> handlePut
            handleGet :: Handler st
            handleGet = liftIO do
                e <- newEmptyTMVarIO
                atomically $ writeTQueue requests (Get e)
                atomically $ takeTMVar e
            handlePut :: st -> Handler NoContent
            handlePut st = liftIO do
                atomically $ writeTQueue requests (Put st)
                pure NoContent
        let clock = constM do
                e <- liftIO . atomically . readTQueue $ requests
                t <- liftIO getCurrentTime
                pure (t, e)
        void . forkIO . Warp.run port . serve rhineApi $ server
        (clock,) <$> getCurrentTime

instance GetClockProxy (RequestClock st)
