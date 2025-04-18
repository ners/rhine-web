{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text qualified as Text
import FRP.Rhine
import FRP.Rhine.Servant
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (methodGet, status200)
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.URI (Query)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (serveDirectoryFileServer)
import Servant.API
import Servant.Client (AsClientT, BaseUrl (..), ClientEnv, ClientM, ResponseF (..), Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (defaultRequest, requestPath)
import Servant.Client.Core.RunClient (runRequest)
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

printState :: (MonadIO m) => ClSF m cl State ()
printState = arrMCl $ liftIO . print

newtype NamedRhineRoutes route = NamedRhineRoutes
    { test :: route :- Get '[JSON] State
    }
    deriving stock (Generic)

data RhineRoutes route = RhineRoutes
    { get :: route :- Get '[JSON] State
    , put :: route :- ReqBody '[JSON] State :> Put '[JSON] NoContent
    , delete :: route :- Delete '[JSON] NoContent
    , queries :: route :- "queries" :> QueryString :> Get '[JSON] String
    , raw :: route :- "raw" :> Raw
    , named :: route :- "named" :> NamedRoutes NamedRhineRoutes
    }
    deriving stock (Generic)

data Routes route = Routes
    { rhine :: route :- "rhine" :> NamedRoutes RhineRoutes
    , files :: route :- Raw
    }
    deriving stock (Generic)

type Api = ToServantApi Routes

apiSf :: forall m. (MonadIO m) => ClSF m (RequestClock RhineRoutes) State State
apiSf = genericServeClSF RhineRoutes{..}
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
    raw :: ClSF m (RouteClock ("raw" :> Raw)) State (State, Wai.Response)
    raw =
        returnA &&& tagS >>> arrMCl \(st, (req, _)) ->
            let name = maybe "world" (fromString . Text.unpack) . listToMaybe $ req.pathInfo
             in pure (st, Wai.responseLBS status200 mempty $ "Hello, " <> name <> "!")
    named :: ClSF m (RequestClock NamedRhineRoutes) State State
    named = genericServeClSF NamedRhineRoutes { test = get }

setup :: IO ([ThreadId], ClientEnv)
setup = do
    let initialState = State{tickCounter = 0, getRequestCounter = 0}
    let port = 8080 :: Int
    let tickRh :: Rhine IO (Millisecond 1) State State
        tickRh = tick @@ waitClock
    let printRh :: Rhine IO (Millisecond 1000) State ()
        printRh = printState @@ waitClock

    clock <- requestClock @RhineRoutes

    let apiRh :: Rhine IO (RequestClock RhineRoutes) State State
        apiRh = apiSf @@ clock

    rhineThread <-
        forkIO . flow $
            feedbackRhine (keepLast initialState) (snd ^>>@ (tickRh |@| apiRh) @>>^ \st -> (st, st))
                >-- keepLast initialState
                --> printRh

    warpThread <-
        forkIO . Warp.run port . genericServe $
            Routes
                { rhine = toHandler @IO clock
                , files = serveDirectoryFileServer "."
                }

    manager <- newManager defaultManagerSettings
    let baseUrl =
            BaseUrl
                { baseUrlScheme = Http
                , baseUrlPort = port
                , baseUrlPath = ""
                , baseUrlHost = "localhost"
                }
    let clientEnv = mkClientEnv manager baseUrl
    pure ([rhineThread, warpThread], clientEnv)

teardown :: ([ThreadId], ClientEnv) -> IO ()
teardown (threads, _) = forM_ threads killThread

withApi :: (ClientEnv -> IO ()) -> IO ()
withApi = bracket setup teardown . (. snd)

apiClient :: Routes (AsClientT ClientM)
apiClient = genericClient

runClientM' :: ClientM a -> ClientEnv -> IO a
runClientM' a e = either (fail . show) pure =<< runClientM a e

main :: IO ()
main = hspec . around withApi . it "works" $ runClientM' do
    runRequest defaultRequest{requestPath = "/rhine-servant.cabal"} >>= \Response{..} -> liftIO do
        responseStatusCode `shouldBe` ok200
        expected <- LazyByteString.readFile "./rhine-servant.cabal"
        responseBody `shouldBe` expected

    apiClient.rhine.raw methodGet >>= \Response{..} -> liftIO do
        responseStatusCode `shouldBe` ok200
        responseBody `shouldBe` "Hello, world!"

    runRequest defaultRequest{requestPath = "/rhine/raw/Rhine"} >>= \Response{..} -> liftIO do
        responseStatusCode `shouldBe` ok200
        responseBody `shouldBe` "Hello, Rhine!"

    liftIO $ threadDelay 1000
    st0 <- apiClient.rhine.get
    liftIO do
        tickCounter st0 `shouldSatisfy` (> tickCounter emptyState)
        getRequestCounter st0 `shouldBe` getRequestCounter emptyState + 1
        threadDelay 1000
    st1 <- apiClient.rhine.named.test
    liftIO do
        tickCounter st1 `shouldSatisfy` (> tickCounter st0)
        getRequestCounter st1 `shouldBe` getRequestCounter st0 + 1
    apiClient.rhine.delete
    st2 <- apiClient.rhine.get
    liftIO do
        tickCounter st2 `shouldSatisfy` (< tickCounter st1)
        getRequestCounter st2 `shouldBe` getRequestCounter emptyState + 1
    let putState = State{tickCounter = 10 ^ (9 :: Int), getRequestCounter = 42}
    apiClient.rhine.put putState
    st3 <- apiClient.rhine.get
    liftIO do
        tickCounter st3 `shouldSatisfy` (>= tickCounter putState)
        getRequestCounter st3 `shouldBe` (getRequestCounter putState + 1)

    let q :: Query
        q = [("This", Just "is"), ("a", Just "query"), ("string", Nothing)]
    r <- apiClient.rhine.queries q
    st4 <- apiClient.rhine.get
    liftIO do
        tickCounter st4 `shouldSatisfy` (>= tickCounter st3)
        getRequestCounter st4 `shouldBe` (getRequestCounter st3 + 1)
        r `shouldBe` show q
