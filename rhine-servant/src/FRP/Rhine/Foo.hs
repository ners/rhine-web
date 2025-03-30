{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module FRP.Rhine.Foo where

import Control.Monad.Schedule.Class
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
  , queries :: route :- QueryString :> ReqBody '[JSON] State :> Put '[JSON] String
  }
  deriving stock (Generic)

type Api = ToServantApi Routes

apiSf :: forall m. (MonadIO m) => ClSF m (RequestClock Routes) State State
apiSf = genericServeClSF Routes{..}
 where
  get :: ClSF m (RouteClock (Get '[JSON] State)) State (State, State)
  get = arr \st -> let st' = st{getRequestCounter = getRequestCounter st + 1} in (st', st')
  put :: ClSF m (RouteClock (ReqBody '[JSON] State :> Put '[JSON] NoContent)) State (State, NoContent)
  put = tagS >>> arr (\((st, ()), _) -> (st, NoContent))
  delete :: ClSF m (RouteClock (Delete '[JSON] NoContent)) State (State, NoContent)
  delete = arr_ (emptyState, NoContent)
  queries :: ClSF m (RouteClock (QueryString :> (ReqBody '[JSON] State :> Put '[JSON] String))) State (State, String)
  queries = tagS >>> arr (\((qs, (st, ())), _) -> (st, show qs))

apiRh :: (MonadIO m, MonadSchedule m) => Rhine m (RequestClock Routes) State State
apiRh = apiSf @@ RequestClock{port = 8080}
