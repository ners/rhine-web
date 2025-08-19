{-# LANGUAGE AllowAmbiguousTypes #-}

module FRP.Rhine.Servant.Internal where

import Data.Kind (Type)
import Servant.Server

type family RouteInput api :: Type

type family RouteOutput api :: Type

class StoreInputs route where
    withInputs :: (RouteInput route -> Handler ()) -> Handler (RouteOutput route) -> Server route
