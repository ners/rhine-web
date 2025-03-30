{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Servant {- (RequestClock (..), RouteClock, (<@|>))-} where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Either (fromLeft, fromRight)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Time (getCurrentTime)
import FRP.Rhine
import GHC.Generics (Generic (..), K1 (..), M1 (..), (:*:) (..))
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (Query)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API
import Servant.API.Modifiers (FoldLenient)
import Servant.Server
import Servant.Server.Generic (AsServer, genericServe)
import Prelude

{-
class RhineRequest api where
    type RhineInput api
    type RhineOutput api

instance RhineRequest (api1 :<|> api2) where
    type RhineInput (api1 :<|> api2) = Either (RhineInput api1) (RhineInput api2)
    type RhineOutput (api1 :<|> api2) = Either (RhineOutput api1) (RhineOutput api2)

instance RhineRequest (Capture' mods capture a :> api) where
    type RhineInput (Capture' mods capture a :> api) = (If (FoldLenient mods) (Either String a) a, RhineInput api)
    type RhineOutput (Capture' mods capture a :> api) = RhineOutput api

instance RhineRequest (CaptureAll capture a :> api) where
    type RhineInput (CaptureAll capture a :> api) = ([a], RhineInput api)
    type RhineOutput (CaptureAll capture a :> api) = RhineOutput api

instance forall (a :: Type) (api :: Type) . RhineRequest (WithResource a :> api) where
    type RhineInput (WithResource a :> api) = ((ReleaseKey, a), RhineInput api)
    type RhineOutput (WithResource a :> api) = RhineOutput api

instance
    forall (method :: StdMethod) (status :: Nat) (ctypes :: [Type]) (a :: Type)
     . RhineRequest (Verb method status ctypes a)
    where
    type RhineInput (Verb method status ctypes a) = ()
    type RhineOutput (Verb method status ctypes a) = a

instance forall (method :: StdMethod). RhineRequest (NoContentVerb method) where
    type RhineInput (NoContentVerb method) = ()
    type RhineOutput (NoContentVerb method) = NoContent

instance
    forall (method :: StdMethod) (status :: Nat) (framing :: Type) (ctype :: Type) (a :: Type)
     . RhineRequest (Stream method status framing ctype a)
    where
    type RhineInput (Stream method status framing ctype a) = ()
    type RhineOutput (Stream method status framing ctype a) = a

instance RhineRequest (Header' mods sym a :> api) where
    type RhineInput (Header' mods sym a :> api) = (RequestArgument mods a, RhineInput api)
    type RhineOutput (Header' mods sym a :> api) = RhineOutput api

instance RhineRequest (QueryParam' mods sym a :> api) where
    type RhineInput (QueryParam' mods sym a :> api) = (RequestArgument mods a, RhineInput api)
    type RhineOutput (QueryParam' mods sym a :> api) = RhineOutput api

instance RhineRequest (QueryParams sym a :> api) where
    type RhineInput (QueryParams sym a :> api) = ([a], RhineInput api)
    type RhineOutput (QueryParams sym a :> api) = RhineOutput api

instance RhineRequest (QueryFlag sym :> api) where
    type RhineInput (QueryFlag sym :> api) = (Bool, RhineInput api)
    type RhineOutput (QueryFlag sym :> api) = RhineOutput api

instance RhineRequest (QueryString :> api) where
    type RhineInput (QueryString :> api) = (Query, RhineInput api)
    type RhineOutput (QueryString :> api) = RhineOutput api

instance RhineRequest (DeepQuery sym a :> api) where
    type RhineInput (DeepQuery sym a :> api) = (a, RhineInput api)
    type RhineOutput (DeepQuery sym a :> api) = RhineOutput api

instance RhineRequest (ReqBody' mods list a :> api) where
    type RhineInput (ReqBody' mods list a :> api) = (If (FoldLenient mods) (Either String a) a, RhineInput api)
    type RhineOutput (ReqBody' mods list a :> api) = RhineOutput api

instance RhineRequest (StreamBody' mods framing ctype a :> api) where
    type RhineInput (StreamBody' mods framing ctype a :> api) = (a, RhineInput api)
    type RhineOutput (StreamBody' mods framing ctype a :> api) = RhineOutput api

instance forall (path :: Symbol) api. RhineRequest (path :> api) where
    type RhineInput (path :> api) = RhineInput api
    type RhineOutput (path :> api) = RhineOutput api

instance RhineRequest (RemoteHost :> api) where
    type RhineInput (RemoteHost :> api) = (SockAddr, RhineInput api)
    type RhineOutput (RemoteHost :> api) = RhineOutput api

instance RhineRequest (IsSecure :> api) where
    type RhineInput (IsSecure :> api) = (IsSecure, RhineInput api)
    type RhineOutput (IsSecure :> api) = RhineOutput api

instance RhineRequest (Vault :> api) where
    type RhineInput (Vault :> api) = (Vault, RhineInput api)
    type RhineOutput (Vault :> api) = RhineOutput api

instance RhineRequest (HttpVersion :> api) where
    type RhineInput (HttpVersion :> api) = (HttpVersion, RhineInput api)
    type RhineOutput (HttpVersion :> api) = RhineOutput api

instance RhineRequest (Summary desc :> api) where
    type RhineInput (Summary desc :> api) = RhineInput api
    type RhineOutput (Summary desc :> api) = RhineOutput api

instance RhineRequest (Description desc :> api) where
    type RhineInput (Description desc :> api) = RhineInput api
    type RhineOutput (Description desc :> api) = RhineOutput api

instance RhineRequest EmptyAPI where
    type RhineInput EmptyAPI = Void
    type RhineOutput EmptyAPI = Void

instance RhineRequest (EmptyAPI :> api) where
    type RhineInput (EmptyAPI :> api) = RhineInput api
    type RhineOutput (EmptyAPI :> api) = RhineOutput api

instance RhineRequest (BasicAuth realm usr :> api) where
    type RhineInput (BasicAuth realm usr :> api) = (usr, RhineInput api)
    type RhineOutput (BasicAuth realm usr :> api) = RhineOutput api

instance RhineRequest (WithNamedContext name subContext subApi) where
    type RhineInput (WithNamedContext name subContext subApi) = RhineInput subApi
    type RhineOutput (WithNamedContext name subContext subApi) = RhineOutput subApi

instance RhineRequest (Fragment a :> api) where
    type RhineInput (Fragment a :> api) = RhineInput api
    type RhineOutput (Fragment a :> api) = RhineOutput api

-- TODO
-- instance RhineRequest (NamedRoutes api) where
--    type RhineInput (NamedRoutes api) = RhineInput api
--    type RhineOutput (NamedRoutes api) = RhineOutput api

data RouteClock route = RouteClock
    { input :: TMVar (RhineInput route)
    , output :: TMVar (RhineOutput route)
    }

instance Clock m (RouteClock route) where
    type Time (RouteClock route) = UTCTime
    type Tag (RouteClock route) = RhineInput route
    initClock = undefined

class (MonadIO m, HasServer api '[], RhineRequest api) => HasRhineRoutes m api where
    type RhineRoutes api

    -- type HandlerContinuation api

    initRoutes :: m (RhineRoutes api)
    default initRoutes :: (RhineRoutes api ~ RouteClock api) => m (RhineRoutes api)
    initRoutes = liftIO do
        input <- newEmptyTMVarIO
        output <- newEmptyTMVarIO
        pure RouteClock{..}

    scheduleRoutes :: RhineRoutes api -> Automaton m () (UTCTime, Tag (RequestClock m api))

    -- \input1 input2 ... -> do
    --    putTMVar (input1, input2, ...)
    --    takeTMVar output

    -- Input1 :?> (Input2 :?> ... :?> GET ...)
    -- handle :: RouteClock api -> Input1 -> (Input2 -> Input3 -> ... -> Server (GET ...))
    handler :: {- HandlerContinuation api -> -} RhineRoutes api -> Server api

instance (MonadIO m) => HasRhineRoutes m EmptyAPI where
    type RhineRoutes EmptyAPI = ()
    initRoutes = pure ()
    scheduleRoutes () =
        -- TODO can we use the Never clock here?
        constM . liftIO . Control.Monad.forever . threadDelay $ 1_000_000_000
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

instance
    forall (m :: Type -> Type) (method :: StdMethod) (status :: Nat) (ctypes :: [Type]) (a :: Type)
     . (MonadIO m, HasServer (Verb method status ctypes a) '[])
    => HasRhineRoutes m (Verb method status ctypes a)
    where
    type RhineRoutes (Verb method status ctypes a) = RouteClock (Verb method status ctypes a)

    scheduleRoutes RouteClock{..} = constM . liftIO $ do
        let o = liftIO . atomically . putTMVar output
        () <- atomically $ takeTMVar input
        t <- getCurrentTime
        pure (t, ((), o))

    handler RouteClock{..} = liftIO $ do
        atomically . putTMVar input $ ()
        atomically . takeTMVar $ output

instance
    forall (m :: Type -> Type) (method :: StdMethod)
     . (MonadIO m, HasServer (NoContentVerb method) '[])
    => HasRhineRoutes m (NoContentVerb method)
    where
    type RhineRoutes (NoContentVerb method) = RouteClock (NoContentVerb method)

    scheduleRoutes RouteClock{..} = constM . liftIO $ do
        let o = liftIO . atomically . putTMVar output
        () <- atomically $ takeTMVar input
        t <- getCurrentTime
        pure (t, ((), o))

    handler RouteClock{..} = liftIO $ do
        atomically . putTMVar input $ ()
        atomically . takeTMVar $ output

instance
    forall (m :: Type -> Type) (method :: StdMethod) (status :: Nat) (framing :: Type) (ctype :: Type) (a :: Type)
     . (MonadIO m, HasServer (Stream method status framing ctype a) '[])
    => HasRhineRoutes m (Stream method status framing ctype a)
    where
    type RhineRoutes (Stream method status framing ctype a) = RouteClock (Stream method status framing ctype a)

    scheduleRoutes RouteClock{..} = constM . liftIO $ do
        let o = liftIO . atomically . putTMVar output
        () <- atomically $ takeTMVar input
        t <- getCurrentTime
        pure (t, ((), o))

    handler RouteClock{..} = liftIO $ do
        atomically . putTMVar input $ ()
        atomically . takeTMVar $ output

instance
    (MonadIO m, RhineRequest (QueryString :> api), HasServer (QueryString :> api) '[], HasRhineRoutes m api)
    => HasRhineRoutes m (QueryString :> api)
    where
    type RhineRoutes (QueryString :> api) = RouteClock (QueryString :> api)

    scheduleRoutes RouteClock{..} = constM . liftIO $ do
        let o = liftIO . atomically . putTMVar output
        i <- atomically $ takeTMVar input
        t <- getCurrentTime
        pure (t, (i, o))

    handler RouteClock{..} = undefined -- liftIO $ do
        -- atomically . takeTMVar $ output

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

(<@|>)
    :: (Monad m)
    => ClSF m (RouteClock route) st (st, RhineOutput route)
    -> ClSF m (RequestClock m api) st st
    -> ClSF m (RequestClock m (route :<|> api)) st st
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
-}

data RouteClock route = RouteClock
    { input :: TMVar (RouteInput route)
    , output :: TMVar (RouteOutput route)
    }

instance Clock m (RouteClock route) where
    type Time (RouteClock route) = UTCTime
    type Tag (RouteClock route) = (RouteInput route, RouteOutput route -> IO ())
    initClock = error "Running RouteClock standalone is not supported"

type family RouteInput api :: Type

type instance
    RouteInput (ReqBody' mods _ a :> api) =
        (If (FoldLenient mods) (Either String a) a, RouteInput api)

type instance RouteInput (QueryString :> api) = (Query, RouteInput api)

type instance RouteInput ((_path :: Symbol) :> api) = RouteInput api

type instance RouteInput (Verb _ _ _ _) = ()

type family RouteOutput api :: Type

type instance RouteOutput (Verb _ _ _ a) = a

type instance RouteOutput (_ :> api) = RouteOutput api

class StoreInputs route where
    withInputs :: (RouteInput route -> Handler ()) -> Handler (RouteOutput route) -> Server route

instance (StoreInputs api) => StoreInputs (ReqBody' mods list a :> api) where
    withInputs store k body = withInputs @api (store . (body,)) k

instance (StoreInputs api) => StoreInputs (QueryString :> api) where
    withInputs store k query = withInputs @api (store . (query,)) k

instance (StoreInputs api) => StoreInputs ((_path :: Symbol) :> api) where
    withInputs = withInputs @api

instance StoreInputs (Verb method status ctypes a) where
    withInputs store k = store () >> k

data AsServerClSF (m :: Type -> Type) (state :: Type)

instance GenericMode (AsServerClSF m state) where
    type
        (AsServerClSF m state) :- api =
            ClSF m (RouteClock api) state (state, RouteOutput api)

data AsRouteClock

instance GenericMode AsRouteClock where
    type AsRouteClock :- api = RouteClock api

class (MonadIO m, MonadSchedule m) => GToInnerClock m f where
    type GInnerClock f
    type GHandler f :: Type -> Type

    initRouteClock :: m (f p)
    scheduleRouteClock :: f p -> RunningClock m UTCTime (Tag (GInnerClock f))

    grouteHandler :: f p -> GHandler f p

instance (GToInnerClock m f) => GToInnerClock m (M1 i c f) where
    type GInnerClock (M1 i c f) = GInnerClock f
    type GHandler (M1 i c f) = M1 i c (GHandler f)

    initRouteClock = M1 <$> initRouteClock
    scheduleRouteClock = scheduleRouteClock . unM1

    grouteHandler = M1 . grouteHandler @m @f . unM1

instance (MonadIO m, MonadSchedule m, StoreInputs api) => GToInnerClock m (K1 i (RouteClock api)) where
    type GInnerClock (K1 i (RouteClock api)) = RouteClock api
    type GHandler (K1 i (RouteClock api)) = K1 i (Server api)

    initRouteClock =
        K1 <$> do
            input <- liftIO newEmptyTMVarIO
            output <- liftIO newEmptyTMVarIO
            pure RouteClock{..}

    scheduleRouteClock (K1 RouteClock{..}) = constM . liftIO $ do
        let o = atomically . putTMVar output
        i <- atomically $ takeTMVar input
        t <- getCurrentTime
        pure (t, (i, o))

    grouteHandler (K1 RouteClock{..}) =
        K1 $
            withInputs @api
                (liftIO . atomically . putTMVar input)
                (liftIO . atomically $ takeTMVar output)

instance (GToInnerClock m l, GToInnerClock m r) => GToInnerClock m (l :*: r) where
    type GInnerClock (l :*: r) = GInnerClock l `ParallelClock` GInnerClock r
    type GHandler (l :*: r) = GHandler l :*: GHandler r

    initRouteClock = (:*:) <$> initRouteClock <*> initRouteClock
    scheduleRouteClock (l :*: r) =
        schedulePair
            (scheduleRouteClock l >>> arr (second Left))
            (scheduleRouteClock r >>> arr (second Right))

    grouteHandler (l :*: r) = grouteHandler @m l :*: grouteHandler @m r

class GClSFCombine m state f where
    type GClockFromClSF f
    gClSFCombine :: f p -> ClSF m (GClockFromClSF f) state state

instance (GClSFCombine m state f) => GClSFCombine m state (M1 i c f) where
    type GClockFromClSF (M1 i c f) = GClockFromClSF f
    gClSFCombine = gClSFCombine . unM1

instance (MonadIO m, output ~ RouteOutput r) => GClSFCombine m state (K1 i (ClSF m (RouteClock r) state (state, output))) where
    type GClockFromClSF (K1 i (ClSF m (RouteClock r) state (state, output))) = RouteClock r
    gClSFCombine (K1 handlerClSF) = proc st -> do
        (_, dispatch) <- tagS -< ()
        (st', o) <- handlerClSF -< st
        arrMCl (uncurry ($)) -< (liftIO . dispatch, o)
        returnA -< st'

instance
    ( Monad m
    , GClSFCombine m state l
    , GClSFCombine m state r
    , Time (GClockFromClSF l) ~ Time (GClockFromClSF r)
    )
    => GClSFCombine m state (l :*: r)
    where
    type GClockFromClSF (l :*: r) = GClockFromClSF l `ParallelClock` GClockFromClSF r
    gClSFCombine (l :*: r) = proc st -> do
        tag <- tagS -< ()
        case tag of
            Left _ -> hoistL (gClSFCombine @m @state l) -< st
            Right _ -> hoistR (gClSFCombine @m @state r) -< st
      where
        hoistL :: ClSF m (GClockFromClSF l) state state -> ClSF m (ParallelClock (GClockFromClSF l) (GClockFromClSF r)) state state
        hoistL = hoistS $ withReaderT . retag $ fromLeft undefined
        hoistR :: ClSF m (GClockFromClSF r) state state -> ClSF m (ParallelClock (GClockFromClSF l) (GClockFromClSF r)) state state
        hoistR = hoistS $ withReaderT . retag $ fromRight undefined

newtype RequestClock (routes :: Type -> Type) = RequestClock {port :: Port}

handler
    :: forall m routes
     . ( Generic (routes AsRouteClock)
       , Generic (routes AsServer)
       , GToInnerClock m (Rep (routes AsRouteClock))
       , GHandler (Rep (routes AsRouteClock)) ~ Rep (routes AsServer)
       )
    => routes AsRouteClock
    -> routes AsServer
handler = to . grouteHandler @m @(Rep (routes AsRouteClock)) . from

instance
    forall m routes
     . ( MonadIO m
       , HasServer (ToServantApi routes) '[]
       , GenericServant routes AsServer
       , Server (ToServantApi routes) ~ ToServant routes AsServer
       , GToInnerClock m (Rep (routes AsRouteClock))
       , Generic (routes AsRouteClock)
       , GHandler (Rep (routes AsRouteClock)) ~ Rep (routes AsServer)
       )
    => Clock m (RequestClock routes)
    where
    type Time (RequestClock routes) = UTCTime
    type Tag (RequestClock routes) = Tag (GInnerClock (Rep (routes AsRouteClock)))
    initClock RequestClock{..} = do
        innerClock <- initRouteClock @_ @(Rep (routes AsRouteClock))
        let clock = scheduleRouteClock innerClock
        time <- liftIO getCurrentTime
        void . liftIO . forkIO . Warp.run port . genericServe @routes $ handler @m (to innerClock)
        pure (clock, time)

instance GetClockProxy (RequestClock routes)

genericServeClSF
    :: forall routes m state
     . ( Generic (routes (AsServerClSF m state))
       , GClSFCombine m state (Rep (routes (AsServerClSF m state)))
       , GClockFromClSF (Rep (routes (AsServerClSF m state))) ~ GInnerClock (Rep (routes AsRouteClock))
       , Monad m
       , Time (GInnerClock (Rep (routes AsRouteClock))) ~ UTCTime
       )
    => routes (AsServerClSF m state)
    -> ClSF m (RequestClock routes) state state
genericServeClSF = embedRequest . gClSFCombine . from
  where
    embedRequest
        :: ClSF m (GInnerClock (Rep (routes AsRouteClock))) state state
        -> ClSF m (RequestClock routes) state state
    embedRequest = hoistS $ withReaderT (retag id)
