module Acme.Signal (
    ignoreSigChild
  , setHandler
  , forkProcess
  ) where

import Control.Monad
import Control.Exception as E
import System.Exit
import System.Posix

ignoreSigChild :: IO ()
ignoreSigChild = void $ installHandler sigCHLD Ignore Nothing

setHandler :: [ProcessID] -> IO ()
setHandler cids = do
    void $ installHandler sigINT handler Nothing
    void $ installHandler sigTERM handler Nothing
  where
    handler = Catch $ do
        mapM_ sendTermSignal cids
        exitImmediately ExitSuccess

sendTermSignal :: ProcessID -> IO ()
sendTermSignal cid = signalProcess sigTERM cid `E.catch` ignore

ignore :: SomeException -> IO ()
ignore _ = return ()
