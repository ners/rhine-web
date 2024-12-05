{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.STM (atomically, putTMVar)
import Data.Aeson (FromJSON, ToJSON)
import FRP.Rhine
import FRP.Rhine.Servant
import GHC.Generics (Generic)
import Servant
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

type Api = Get '[JSON] State :<|> ReqBody '[JSON] State :> Put '[JSON] NoContent :<|> Delete '[JSON] NoContent

handleRequest :: (MonadIO m) => ClSF m (RequestClock Api) State State
handleRequest =
    returnA &&& tagS >>> arrMCl \(st, req) ->
        case req of
            Left (GetRequest tmvar) -> do
                let newState = st{getRequestCounter = getRequestCounter st + 1}
                liftIO . atomically $ putTMVar tmvar newState
                pure newState
            Right (Left (PutRequest newState)) -> pure newState
            Right (Right DeleteRequest) -> pure State{tickCounter = 0, getRequestCounter = 0}

getRequestRh :: (MonadIO m) => Rhine m (RequestClock Api) State State
getRequestRh = handleRequest @@ RequestClock{port = 8080}

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
