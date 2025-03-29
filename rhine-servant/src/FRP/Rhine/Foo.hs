{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module FRP.Rhine.Foo where

import Data.Aeson (FromJSON, ToJSON)
import FRP.Rhine
import FRP.Rhine.Servant
import GHC.Generics (Generic (..))
import Servant
import Prelude

data State = State
    { tickCounter :: Int
    , getRequestCounter :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

emptyState :: State
emptyState = State{tickCounter = 0, getRequestCounter = 0}

data Routes route = Routes
    { get :: route :- Get '[JSON] State
    , put :: route :- ReqBody '[JSON] State :> Put '[JSON] NoContent
    , delete :: route :- Delete '[JSON] NoContent
    }
    deriving stock (Generic)

type Api = ToServantApi Routes

apiSf :: forall m. (MonadIO m) => ClSF m (RequestClock Routes) State State
apiSf = genericServeClSF Routes{get = getSf, put = putSf, delete = deleteSf}
  where
    getSf :: ClSF m (RouteClock (Get '[JSON] State)) State (State, State)
    getSf = arr \st -> let st' = st{getRequestCounter = getRequestCounter st + 1} in (st', st')
    putSf :: ClSF m (RouteClock (ReqBody '[JSON] State :> Put '[JSON] NoContent)) State (State, NoContent)
    putSf = tagS >>> arr (second $ const NoContent)
    deleteSf :: ClSF m (RouteClock (Delete '[JSON] NoContent)) State (State, NoContent)
    deleteSf = arr_ (emptyState, NoContent)

apiRh :: (MonadIO m) => Rhine m (RequestClock Routes) State State
apiRh = apiSf @@ RequestClock{port = 8080}
