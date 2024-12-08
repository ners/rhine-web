{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Servant (RequestClock (..), RouteClock, (<@|>)) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Either (fromLeft, fromRight)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Time (getCurrentTime)
import Data.Void (Void)
import FRP.Rhine
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API (Delete, EmptyAPI, Get, Post, Put, ReqBody, (:<|>) (..), (:>))
import Servant.Server
import Prelude

class RhineRequest api where
    type RhineInput api
    type RhineOutput api

instance RhineRequest EmptyAPI where
    type RhineInput EmptyAPI = Void
    type RhineOutput EmptyAPI = Void

instance RhineRequest (api1 :<|> api2) where
    type RhineInput (api1 :<|> api2) = Either (RhineInput api1) (RhineInput api2)
    type RhineOutput (api1 :<|> api2) = Either (RhineOutput api1) (RhineOutput api2)

instance RhineRequest (Get contentTypes output) where
    type RhineInput (Get contentTypes output) = ()
    type RhineOutput (Get contentTypes output) = output

instance RhineRequest (ReqBody contentTypes input :> Put contentTypes' output) where
    type RhineInput (ReqBody contentTypes input :> Put contentTypes' output) = input
    type RhineOutput (ReqBody contentTypes input :> Put contentTypes' output) = output

instance RhineRequest (ReqBody contentTypes input :> Post contentTypes' output) where
    type RhineInput (ReqBody contentTypes input :> Post contentTypes' output) = input
    type RhineOutput (ReqBody contentTypes input :> Post contentTypes' output) = output

instance RhineRequest (Delete contentTypes output) where
    type RhineInput (Delete contentTypes output) = ()
    type RhineOutput (Delete contentTypes output) = output

data RouteClock route = RouteClock {input :: TMVar (RhineInput route), output :: TMVar (RhineOutput route)}

instance Clock m (RouteClock route) where
    type Time (RouteClock route) = UTCTime
    type Tag (RouteClock route) = RhineInput route
    initClock = undefined

class (MonadIO m, HasServer api '[], RhineRequest api) => HasRhineRoutes m api where
    type RhineRoutes api

    initRoutes :: m (RhineRoutes api)
    default initRoutes :: (RhineRoutes api ~ RouteClock api) => m (RhineRoutes api)
    initRoutes = liftIO do
        input <- newEmptyTMVarIO
        output <- newEmptyTMVarIO
        pure RouteClock{..}

    scheduleRoutes :: RhineRoutes api -> Automaton m () (UTCTime, Tag (RequestClock m api))
    default scheduleRoutes :: (RhineRoutes api ~ RouteClock api) => RhineRoutes api -> Automaton m () (UTCTime, Tag (RequestClock m api))
    scheduleRoutes RouteClock{..} = constM . liftIO $ do
        i <- atomically . takeTMVar $ input
        let o = liftIO . atomically . putTMVar output
        t <- getCurrentTime
        pure (t, (i, o))

    handler :: RhineRoutes api -> Server api

instance (MonadIO m) => HasRhineRoutes m EmptyAPI where
    type RhineRoutes EmptyAPI = ()
    initRoutes = pure ()
    scheduleRoutes () = constM . liftIO . forever . threadDelay $ 1_000_000_000
    handler () = emptyServer

instance (MonadSchedule m, HasRhineRoutes m api1, HasRhineRoutes m api2) => HasRhineRoutes m (api1 :<|> api2) where
    type RhineRoutes (api1 :<|> api2) = (RhineRoutes api1, RhineRoutes api2)
    initRoutes = (,) <$> initRoutes @m @api1 <*> initRoutes @m @api2

    scheduleRoutes (r1, r2) = schedulePair (second dispatchL <$> scheduleRoutes @m @api1 r1) (second dispatchR <$> scheduleRoutes @m @api2 r2)
      where
        dispatchL :: (i1, o1 -> m ()) -> (Either i1 i2, Either o1 o2 -> m ())
        dispatchL (i, dispatch) = (Left i, dispatch . fromLeft undefined)
        dispatchR :: (i2, o2 -> m ()) -> (Either i1 i2, Either o1 o2 -> m ())
        dispatchR (i, dispatch) = (Right i, dispatch . fromRight undefined)

    handler (r1, r2) = handler @m @api1 r1 :<|> handler @m @api2 r2

instance (MonadIO m, HasServer (Get contentTypes output) '[]) => HasRhineRoutes m (Get contentTypes output) where
    type RhineRoutes (Get contentTypes output) = RouteClock (Get contentTypes output)
    handler RouteClock{..} = liftIO do
        atomically . putTMVar input $ ()
        atomically . takeTMVar $ output

instance (MonadIO m, HasServer (ReqBody contentTypes input :> Put contentTypes' output) '[]) => HasRhineRoutes m (ReqBody contentTypes input :> Put contentTypes' output) where
    type RhineRoutes (ReqBody contentTypes input :> Put contentTypes' output) = RouteClock (ReqBody contentTypes input :> Put contentTypes' output)
    handler RouteClock{..} i = liftIO do
        atomically . putTMVar input $ i
        atomically . takeTMVar $ output

instance (MonadIO m, HasServer (ReqBody contentTypes input :> Post contentTypes' output) '[]) => HasRhineRoutes m (ReqBody contentTypes input :> Post contentTypes' output) where
    type RhineRoutes (ReqBody contentTypes input :> Post contentTypes' output) = RouteClock (ReqBody contentTypes input :> Post contentTypes' output)
    handler RouteClock{..} i = liftIO do
        atomically . putTMVar input $ i
        atomically . takeTMVar $ output

instance (MonadIO m, HasServer (Delete contentTypes output) '[]) => HasRhineRoutes m (Delete contentTypes output) where
    type RhineRoutes (Delete contentTypes output) = RouteClock (Delete contentTypes output)
    handler RouteClock{..} = liftIO do
        atomically . putTMVar input $ ()
        atomically . takeTMVar $ output

newtype RequestClock (m :: Type -> Type) api = RequestClock {port :: Port}

instance (MonadIO m, HasRhineRoutes m api) => Clock m (RequestClock m api) where
    type Time (RequestClock m api) = UTCTime
    type Tag (RequestClock m api) = (RhineInput api, RhineOutput api -> m ())
    initClock RequestClock{..} = do
        routes <- initRoutes @m @api
        let clock = scheduleRoutes @m @api routes
        time <- liftIO getCurrentTime
        void . liftIO . forkIO . Warp.run port . serve (Proxy :: Proxy api) $ handler @m @api routes
        pure (clock, time)

instance GetClockProxy (RequestClock m api)

infixr 3 <@|>

(<@|>) :: (Monad m) => ClSF m (RouteClock route) st (st, RhineOutput route) -> ClSF m (RequestClock m api) st st -> ClSF m (RequestClock m (route :<|> api)) st st
handler' <@|> rest = proc st -> do
    (tag, dispatch) <- tagS -< ()
    case tag of
        Left _ -> do
            (st', o) <- hoistRouteClock handler' -< st
            arrMCl (\(dispatch, o) -> dispatch $ Left o) -< (dispatch, o)
            returnA -< st'
        Right _ -> hoistRequestClock rest -< st
  where
    hoistRouteClock :: (Monad m) => ClSF m (RouteClock route) a b -> ClSF m (RequestClock m (route :<|> api)) a b
    hoistRouteClock = hoistS $ withReaderT . retag $ fromLeft undefined . fst
    hoistRequestClock :: (Monad m) => ClSF m (RequestClock m api) a b -> ClSF m (RequestClock m (route :<|> api)) a b
    hoistRequestClock = hoistS $ withReaderT . retag $ \(i, dispatch) -> (fromRight undefined i, dispatch . Right)
