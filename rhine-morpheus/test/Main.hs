module Main where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import FRP.Rhine
import FRP.Rhine.Morpheus
import Test.Hspec
import Prelude

main :: IO ()
main = hspec . it "works" $ pure @IO ()
