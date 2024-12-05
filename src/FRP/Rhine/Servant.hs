{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Servant where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Functor (void)
import Data.Proxy (Proxy (Proxy))
import Data.Time (getCurrentTime)
import FRP.Rhine
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API (Delete, Get, NoContent (NoContent), Put, ReqBody, (:<|>) (..), (:>))
import Servant.Server
import Prelude

class HasRhineRequest api where
    type RhineRequest api

newtype RhineRequestAlternative a b = RhineRequestAlternative (Either (RhineRequest a) (RhineRequest b))

instance HasRhineRequest (a :<|> b) where
    type RhineRequest (a :<|> b) = Either (RhineRequest a) (RhineRequest b)

type family RhineHandler api where
    RhineHandler (Get _ output) = Handler output
    RhineHandler (ReqBody _ input :> Put _ NoContent) = input -> Handler NoContent
    RhineHandler (Delete _ NoContent) = Handler NoContent
    RhineHandler (api1 :<|> api2) = RhineHandler api1 :<|> RhineHandler api2

class RhineServer api where
    server :: (RhineRequest api -> IO ()) -> Server api
    default server :: (RhineHandler api ~ Server api) => (RhineRequest api -> IO ()) -> Server api
    server = handler @api
    handler :: (RhineRequest api -> IO ()) -> RhineHandler api

instance RhineServer (Get contentTypes a) where
    handler dispatch = liftIO do
        var <- newEmptyTMVarIO
        dispatch $ GetRequest var
        atomically $ takeTMVar var

newtype GetRequest a = GetRequest (TMVar a)

instance HasRhineRequest (Get contentTypes a) where
    type RhineRequest (Get contentTypes a) = GetRequest a

newtype PutRequest b = PutRequest b

instance HasRhineRequest (ReqBody contentTypes input :> Put contentTypes' NoContent) where
    type RhineRequest (ReqBody contentTypes input :> Put contentTypes' NoContent) = PutRequest input

instance RhineServer (ReqBody contentTypes input :> Put contentTypes' NoContent) where
    handler dispatch input = liftIO do
        dispatch $ PutRequest input
        pure NoContent

data DeleteRequest = DeleteRequest

instance HasRhineRequest (Delete contentTypes' NoContent) where
    type RhineRequest (Delete contentTypes' NoContent) = DeleteRequest

instance RhineServer (Delete contentTypes' NoContent) where
    handler dispatch = liftIO do
        dispatch DeleteRequest
        pure NoContent

instance (RhineHandler a ~ Server a, RhineHandler b ~ Server b, RhineServer a, RhineServer b) => RhineServer (a :<|> b) where
    handler dispatch = (handler @a $ dispatch . Left) :<|> (handler @b $ dispatch . Right)

newtype RequestClock api = RequestClock {port :: Port}

instance (MonadIO m, HasServer api '[], RhineServer api, Server api ~ RhineHandler api) => Clock m (RequestClock api) where
    type Time (RequestClock api) = UTCTime
    type Tag (RequestClock api) = RhineRequest api
    initClock RequestClock{..} = liftIO do
        requests <- newTQueueIO
        let clock = constM do
                e <- liftIO . atomically . readTQueue $ requests
                t <- liftIO getCurrentTime
                pure (t, e)
        void . forkIO . Warp.run port . serve (Proxy :: Proxy api) . handler @api $ atomically . writeTQueue requests
        (clock,) <$> getCurrentTime

instance GetClockProxy (RequestClock api)
