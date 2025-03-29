{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import FRP.Rhine
import FRP.Rhine.Servant
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientEnv, ClientError, ClientM, EmptyClient (..), Scheme (..), client, mkClientEnv, runClientM)
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
    }

type Api = ToServantApi Routes

apiSf :: forall m. (MonadIO m) => ClSF m (RequestClock m Api) State State
apiSf = genericServeClSF Routes{get = getSf, put = putSf, delete = deleteSf}
  where
    getSf :: ClSF m (RouteClock (Get '[JSON] State)) State (State, State)
    getSf = arr \st -> let st' = st{getRequestCounter = getRequestCounter st + 1} in (st', st')
    putSf :: ClSF m (RouteClock (ReqBody '[JSON] State :> Put '[JSON] NoContent)) State (State, NoContent)
    putSf = tagS >>> arr (,NoContent)
    deleteSf :: ClSF m (RouteClock (Delete '[JSON] NoContent)) State (State, NoContent)
    deleteSf = arr_ (emptyState, NoContent)

startApi :: IO Void
startApi = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}
    let port = 8080 :: Int
    let tickRh :: (Monad m) => Rhine m (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock
    let apiRh :: (MonadIO m) => Rhine m (RequestClock m Api) State State
        apiRh = apiSf @@ RequestClock{..}
    flow $
        feedbackRhine (keepLast initialState) (snd ^>>@ (tickRh |@| apiRh) @>>^ \st -> (st, st))
            >-- keepLast initialState
            --> printRh

setup :: IO (ThreadId, ClientEnv)
setup = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}
    let port = 8080 :: Int
    let tickRh :: (Monad m) => Rhine m (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock
    let apiRh :: (MonadIO m) => Rhine m (RequestClock m Api) State State
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

main :: IO ()
main =
    hspec . around withApi . it "works" $
        fmap (either (error . show) id) . runClientM do
            st0 <- getM
            liftIO do
                tickCounter st0 `shouldSatisfy` (> tickCounter emptyState)
                getRequestCounter st0 `shouldBe` getRequestCounter emptyState + 1
                threadDelay 1000
            st1 <- getM
            liftIO do
                tickCounter st1 `shouldSatisfy` (> tickCounter st0)
                getRequestCounter st1 `shouldBe` getRequestCounter st0 + 1
            deleteM
            st2 <- getM
            liftIO do
                tickCounter st2 `shouldSatisfy` (< tickCounter st1)
                getRequestCounter st2 `shouldBe` getRequestCounter emptyState + 1
            let putState = State{tickCounter = 10 ^ 9, getRequestCounter = 42}
            putM putState
            st3 <- getM
            liftIO do
                tickCounter st3 `shouldSatisfy` (>= tickCounter putState)
                getRequestCounter st3 `shouldBe` (getRequestCounter putState + 1)
