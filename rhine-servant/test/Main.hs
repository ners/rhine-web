{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Schedule.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import FRP.Rhine
import FRP.Rhine.Servant
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.URI (Query)
import Servant.API
import Servant.Client
    ( AsClientT
    , BaseUrl (..)
    , ClientEnv
    , ClientM
    , Scheme (..)
    , mkClientEnv
    , runClientM
    )
import Servant.Client.Generic (genericClient)
import Test.Hspec
import Prelude

data State = State
    { tickCounter :: Int
    , getRequestCounter :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

emptyState :: State
emptyState = State{tickCounter = 0, getRequestCounter = 0}

tick :: (Monad m) => ClSF m cl State State
tick = arr \st -> st{tickCounter = tickCounter st + 1}

printState :: ClSF IO cl State ()
printState = arrMCl print

data Routes route = Routes
    { get :: route :- Get '[JSON] State
    , put :: route :- ReqBody '[JSON] State :> Put '[JSON] NoContent
    , delete :: route :- Delete '[JSON] NoContent
    , queries :: route :- "queries" :> QueryString :> Get '[JSON] String
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
    queries :: ClSF m (RouteClock ("queries" :> QueryString :> Get '[JSON] String)) State (State, String)
    queries = proc st -> do
        ((qs, ()), _) <- tagS -< ()
        returnA -< (st, show qs)

startApi :: IO Void
startApi = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}
    let port = 8080 :: Int
    let tickRh :: Rhine IO (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock
    let apiRh :: (MonadIO m, MonadSchedule m) => Rhine m (RequestClock Routes) State State
        apiRh = apiSf @@ RequestClock{..}
    flow $
        feedbackRhine (keepLast initialState) (snd ^>>@ (tickRh |@| apiRh) @>>^ \st -> (st, st))
            >-- keepLast initialState
            --> printRh

setup :: IO (ThreadId, ClientEnv)
setup = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}
    let port = 8080 :: Int
    let tickRh :: Rhine IO (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock
    let apiRh :: (MonadIO m, MonadSchedule m) => Rhine m (RequestClock Routes) State State
        apiRh = apiSf @@ RequestClock{..}
    threadId <-
        forkIO . flow $
            feedbackRhine (keepLast initialState) (snd ^>>@ (tickRh |@| apiRh) @>>^ \st -> (st, st))
                >-- keepLast initialState
                --> printRh
    manager <- newManager defaultManagerSettings
    let baseUrl =
            BaseUrl
                { baseUrlScheme = Http
                , baseUrlPort = port
                , baseUrlPath = ""
                , baseUrlHost = "localhost"
                }
    let clientEnv = mkClientEnv manager baseUrl
    pure (threadId, clientEnv)

teardown :: (ThreadId, ClientEnv) -> IO ()
teardown (threadId, _) = killThread threadId

withApi :: (ClientEnv -> IO ()) -> IO ()
withApi = bracket setup teardown . (. snd)

apiClient :: Routes (AsClientT ClientM)
apiClient = genericClient

main :: IO ()
main =
    hspec . around withApi . it "works" $
        fmap (either (error . show) id) . runClientM do
            st0 <- apiClient.get
            liftIO do
                tickCounter st0 `shouldSatisfy` (> tickCounter emptyState)
                getRequestCounter st0 `shouldBe` getRequestCounter emptyState + 1
                threadDelay 1000
            st1 <- apiClient.get
            liftIO do
                tickCounter st1 `shouldSatisfy` (> tickCounter st0)
                getRequestCounter st1 `shouldBe` getRequestCounter st0 + 1
            apiClient.delete
            st2 <- apiClient.get
            liftIO do
                tickCounter st2 `shouldSatisfy` (< tickCounter st1)
                getRequestCounter st2 `shouldBe` getRequestCounter emptyState + 1
            let putState = State{tickCounter = 10 ^ 9, getRequestCounter = 42}
            apiClient.put putState
            st3 <- apiClient.get
            liftIO do
                tickCounter st3 `shouldSatisfy` (>= tickCounter putState)
                getRequestCounter st3 `shouldBe` (getRequestCounter putState + 1)

            let q :: Query
                q = [("This", Just "is"), ("a", Just "query"), ("string", Nothing)]
            r <- apiClient.queries q
            st4 <- apiClient.get
            liftIO do
                tickCounter st4 `shouldSatisfy` (>= tickCounter st3)
                getRequestCounter st4 `shouldBe` (getRequestCounter st3 + 1)
                r `shouldBe` show q
