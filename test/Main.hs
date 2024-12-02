{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.STM (atomically, putTMVar)
import Data.Aeson (FromJSON, ToJSON)
import FRP.Rhine
import FRP.Rhine.Servant (Request (..), RequestClock (..))
import GHC.Generics (Generic)
import Prelude

data State = State
    { tickCounter :: Int
    , getRequestCounter :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

tick :: (Monad m) => ClSF m cl State State
tick = arr \st -> st{tickCounter = tickCounter st + 1}

printState :: ClSF IO cl State ()
printState = arrMCl print

getRequest :: (MonadIO m) => ClSF m (RequestClock State) State State
getRequest =
    returnA &&& tagS >>> arrMCl \(st, req) ->
        case req of
            Get tmvar -> do
                let newState = st{getRequestCounter = getRequestCounter st + 1}
                liftIO . atomically $ putTMVar tmvar newState
                pure newState
            Put newState -> do
                pure newState

getRequestRh :: (MonadIO m) => Rhine m (RequestClock State) State State
getRequestRh = getRequest @@ RequestClock{port = 8080}

main :: IO ()
main = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}

    let tickRh :: (Monad m) => Rhine m (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock
    flow $
        feedbackRhine (keepLast initialState) (snd ^>>@ (tickRh |@| getRequestRh) @>>^ \st -> (st, st))
            >-- keepLast initialState
            --> printRh
