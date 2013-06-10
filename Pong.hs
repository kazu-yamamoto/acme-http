{-# Language OverloadedStrings #-}
module Main where

import Acme.Serve
import Acme.Types
import Data.ByteString.Char8 ()
import System.Environment

main :: IO ()
main = do
    as <- getArgs
    let workers = case as of
            []  -> 0
            a:_ -> read a
    serve workers 8000 pong

pong :: Request -> IO Response
pong _ =
    return $ PongResponse

pong2 :: Request -> IO Response
pong2 _ =
    return $ ByteStringResponse { rsCode = 200
                                , rsHeaders = [ ("Content-Length", "4")
                                              , ("Content-Type"  , "text/plain")
                                              ]
                                , rsBody    = "PONG"
                                }


