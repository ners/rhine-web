{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Servant
    ( RequestClock (..)
    , RouteClock
    , genericServe
    , genericServeClSF
    , requestClock
    , toHandler
    )
where

import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Reader (withReaderT)
import Control.Monad.Trans.Resource (ReleaseKey)
import Data.Either (fromLeft, fromRight)
import Data.Kind (Type)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import FRP.Rhine
import GHC.Generics (Generic (..), K1 (..), M1 (..), (:*:) (..))
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (Query)
import Network.Socket (SockAddr)
import Network.Wai qualified as Wai
import Servant.API
import Servant.API.Modifiers (FoldLenient, RequestArgument)
import Servant.Server
import Servant.Server.Generic (AsServer, genericServe)
import Prelude

type family RouteInput api :: Type

type family RouteOutput api :: Type

class StoreInputs route where
    withInputs :: (RouteInput route -> Handler ()) -> Handler (RouteOutput route) -> Server route

type instance RouteInput Raw = Wai.Request

type instance RouteOutput Raw = Wai.Response

instance StoreInputs Raw where
    withInputs :: (Wai.Request -> Handler ()) -> Handler Wai.Response -> Server Raw
    withInputs store k = Tagged $ \req k' -> either throwIO k' =<< runHandler (store req >> k)

type instance RouteInput (Capture' mods _ a :> api) = (If (FoldLenient mods) (Either String a) a, RouteInput api)

instance (StoreInputs api) => StoreInputs (Capture' mods capture a :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (CaptureAll _ a :> api) = ([a], RouteInput api)

instance (StoreInputs api) => StoreInputs (CaptureAll sym a :> api) where
    withInputs store k as = withInputs @api (store . (as,)) k

type instance RouteInput (WithResource a :> api) = ((ReleaseKey, a), RouteInput api)

instance (StoreInputs api) => StoreInputs (WithResource (a :: Type) :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (Verb _ _ _ _) = ()

type instance RouteOutput (Verb _ _ _ a) = a

instance StoreInputs (Verb method status ctypes a) where
    withInputs store k = store () >> k

type instance RouteInput (NoContentVerb _) = ()

type instance RouteOutput (NoContentVerb _) = NoContent

instance StoreInputs (NoContentVerb method) where
    withInputs store k = store () >> k

type instance RouteInput (Stream _ _ _ _ _) = ()

type instance RouteOutput (Stream _ _ _ _ a) = a

instance StoreInputs (Stream method status framing contentType a) where
    withInputs store k = store () >> k

type instance RouteInput (Header' mods _ a :> api) = (RequestArgument mods a, RouteInput api)

instance (StoreInputs api) => StoreInputs (Header' mods sym a :> api) where
    withInputs store k h = withInputs @api (store . (h,)) k

type instance RouteInput (QueryParam' mods _ a :> api) = (RequestArgument mods a, RouteInput api)

instance (StoreInputs api) => StoreInputs (QueryParam' mods sym a :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (QueryParams _ a :> api) = ([a], RouteInput api)

instance (StoreInputs api) => StoreInputs (QueryParams sym a :> api) where
    withInputs store k as = withInputs @api (store . (as,)) k

type instance RouteInput (QueryFlag _ :> api) = (Bool, RouteInput api)

instance (StoreInputs api) => StoreInputs (QueryFlag sym :> api) where
    withInputs store k b = withInputs @api (store . (b,)) k

type instance RouteInput (QueryString :> api) = (Query, RouteInput api)

instance (StoreInputs api) => StoreInputs (QueryString :> api) where
    withInputs store k query = withInputs @api (store . (query,)) k

type instance RouteInput (DeepQuery _ a :> api) = (a, RouteInput api)

instance (StoreInputs api) => StoreInputs (DeepQuery sym a :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (ReqBody' mods _ a :> api) = (If (FoldLenient mods) (Either String a) a, RouteInput api)

instance (StoreInputs api) => StoreInputs (ReqBody' mods list a :> api) where
    withInputs store k body = withInputs @api (store . (body,)) k

type instance RouteInput (StreamBody' _ _ _ a :> api) = (a, RouteInput api)

instance (StoreInputs api) => StoreInputs (StreamBody' mods framing contentType a :> api) where
    withInputs store k body = withInputs @api (store . (body,)) k

type instance RouteInput ((_path :: Symbol) :> api) = RouteInput api

instance (StoreInputs api) => StoreInputs ((_path :: Symbol) :> api) where
    withInputs = withInputs @api

type instance RouteInput (RemoteHost :> api) = (SockAddr, RouteInput api)

instance (StoreInputs api) => StoreInputs (RemoteHost :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (IsSecure :> api) = (IsSecure, RouteInput api)

instance (StoreInputs api) => StoreInputs (IsSecure :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (Vault :> api) = (Vault, RouteInput api)

instance (StoreInputs api) => StoreInputs (Vault :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (HttpVersion :> api) = (HttpVersion, RouteInput api)

instance (StoreInputs api) => StoreInputs (HttpVersion :> api) where
    withInputs store k a = withInputs @api (store . (a,)) k

type instance RouteInput (Summary _ :> api) = RouteInput api

instance (StoreInputs api) => StoreInputs (Summary desc :> api) where
    withInputs = withInputs @api

type instance RouteInput (Description _ :> api) = RouteInput api

instance (StoreInputs api) => StoreInputs (Description desc :> api) where
    withInputs = withInputs @api

type instance RouteInput EmptyAPI = Void

type instance RouteOutput EmptyAPI = Void

instance StoreInputs EmptyAPI where
    withInputs _ _ = emptyServer

type instance RouteInput (EmptyAPI :> api) = RouteInput api

instance (StoreInputs api) => StoreInputs (EmptyAPI :> api) where
    withInputs = withInputs @api

type instance RouteInput (BasicAuth _ usr :> api) = (usr, RouteInput api)

instance (StoreInputs api) => StoreInputs (BasicAuth realm usr :> api) where
    withInputs store k usr = withInputs @api (store . (usr,)) k

type instance RouteInput (WithNamedContext _ _ subApi) = RouteInput subApi

type instance RouteOutput (WithNamedContext _ _ subApi) = RouteOutput subApi

instance (StoreInputs subApi) => StoreInputs (WithNamedContext name subContext subApi) where
    withInputs = withInputs @subApi

type instance RouteInput (Fragment _ :> api) = RouteInput api

instance (StoreInputs api) => StoreInputs (Fragment a1 :> api) where
    withInputs = withInputs @api

type instance RouteOutput (_ :> api) = RouteOutput api

data RouteClock route = RouteClock
    { input :: TMVar (RouteInput route)
    , output :: TMVar (RouteOutput route)
    }

instance Clock m (RouteClock route) where
    type Time (RouteClock route) = UTCTime
    type Tag (RouteClock route) = (RouteInput route, RouteOutput route -> IO ())
    initClock = error "Running RouteClock standalone is not supported"

data AsServerClSF (m :: Type -> Type) (state :: Type)

type family AsServerClSFT' m state path api where
    AsServerClSFT' m state _ (NamedRoutes routes) = ClSF m (RequestClock routes) state state
    AsServerClSFT' m state path api = ClSF m (RouteClock (ReconstitutePath path api)) state (state, RouteOutput (ReconstitutePath path api))

type family AsServerClSFT m state api where
    AsServerClSFT m state api = AsServerClSFT' m state (CollectPath api) (RemovePath api)

data KindProxy (a :: k)

type family CollectPath (api :: Type) :: [Type] where
    CollectPath (path :> api) = KindProxy path ': CollectPath api
    CollectPath _ = '[]

type family RemovePath api where
    RemovePath (_ :> api) = RemovePath api
    RemovePath api = api

type family ReconstitutePath path api where
    ReconstitutePath (KindProxy p ': p') api = p :> ReconstitutePath p' api
    ReconstitutePath '[] api = api

instance GenericMode (AsServerClSF m state) where
    type
        (AsServerClSF m state) :- api =
            AsServerClSFT m state api

data AsRouteClock

type family AsRouteClockT' path api where
    AsRouteClockT' _ (NamedRoutes routes) = RequestClock routes
    AsRouteClockT' path api = RouteClock (ReconstitutePath path api)

type family AsRouteClockT api where
    AsRouteClockT api = AsRouteClockT' (CollectPath api) (RemovePath api)

instance GenericMode AsRouteClock where
    type AsRouteClock :- api = AsRouteClockT api

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

    initRouteClock = do
        input <- liftIO newEmptyTMVarIO
        output <- liftIO newEmptyTMVarIO
        pure $ K1 RouteClock{..}

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

instance
    ( MonadIO m
    , MonadSchedule m
    , Generic (routes AsServer)
    , Generic (routes AsRouteClock)
    , GToInnerClock m (Rep (routes AsRouteClock))
    , GHandler (Rep (routes AsRouteClock)) ~ Rep (routes AsServer)
    )
    => GToInnerClock m (K1 i (RequestClock routes))
    where
    type GInnerClock (K1 i (RequestClock routes)) = RequestClock routes
    type GHandler (K1 i (RequestClock routes)) = K1 i (routes AsServer)

    initRouteClock = K1 <$> requestClock
    scheduleRouteClock (K1 RequestClock{..}) = scheduleRouteClock . from $ innerClock
    grouteHandler (K1 cl) = K1 $ toHandler @m cl

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
        arrMCl liftIO -< dispatch o
        returnA -< st'

instance GClSFCombine m state (K1 i (ClSF m (RequestClock routes) state state)) where
    type GClockFromClSF (K1 i (ClSF m (RequestClock routes) state state)) = RequestClock routes
    gClSFCombine (K1 requestClSF) = requestClSF

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
        hoistL = hoistS $ withReaderT . retag . fromLeft $ undefined
        hoistR :: ClSF m (GClockFromClSF r) state state -> ClSF m (ParallelClock (GClockFromClSF l) (GClockFromClSF r)) state state
        hoistR = hoistS $ withReaderT . retag . fromRight $ undefined

newtype RequestClock (routes :: Type -> Type) = RequestClock {innerClock :: routes AsRouteClock}

toHandler
    :: forall m routes
     . ( Generic (routes AsRouteClock)
       , Generic (routes AsServer)
       , GToInnerClock m (Rep (routes AsRouteClock))
       , GHandler (Rep (routes AsRouteClock)) ~ Rep (routes AsServer)
       )
    => RequestClock routes
    -> routes AsServer
toHandler RequestClock{..} = to . grouteHandler @m . from $ innerClock

requestClock
    :: forall routes m
     . ( GToInnerClock m (Rep (routes AsRouteClock))
       , Generic (routes AsRouteClock)
       )
    => m (RequestClock routes)
requestClock = do
    innerClock <- to <$> initRouteClock @_ @(Rep (routes AsRouteClock))
    pure RequestClock{..}

instance
    forall m routes
     . ( MonadIO m
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
        let clock = scheduleRouteClock . from $ innerClock
        time <- liftIO getCurrentTime
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
