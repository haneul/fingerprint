{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad  
import Data.Maybe
import Network
import Network.BSD
import System.Environment
import System.Exit
import System.IO
import System.Timeout
import Data.Text
import Control.Applicative

data Record = Record { ip   :: String
                     , port :: Int
                     , log  :: String }
   deriving Show

-- | (port, ms, scan)
scanOpts = [(22,  500, scanSSH)
           ,(80, 1500, scanHTTP)]

main :: IO ()
main = do
    getArgs >>= mapM_ (withSocketsDo . doScan)

doScan :: String -> IO ()
doScan host =
    forM_ scanOpts $ \(port, ms, scan) ->
      threadWithChannel (withDefault Nothing $ scan host port) ms >>= hitCheck
  where
    hitCheck mvar = takeMVar mvar >>= maybe (return ()) printHit
    printHit = putStrLn . show

threadWithChannel :: IO (Maybe Record) -> Int -> IO (MVar (Maybe Record))
threadWithChannel action ms = do
    mvar <- newEmptyMVar
    forkIO $ limit (ms*1000) action mvar
    return mvar
  where limit us action mvar = do
            timeout us action >>= 
              maybe (putMVar mvar Nothing) (putMVar mvar)
              
scanSSH :: String -> Int -> IO (Maybe Record)
scanSSH host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    log <- trim <$> hGetLine h
    hClose h
    return $ Just (Record host port log)

scanHTTP :: String -> Int -> IO (Maybe Record)
scanHTTP host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hPutStr h "GET / HTTP/1.1\n\
              \User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0\n\
              \Host: xk\n\
              \Accept: */*\n\n"
    -- get until "Server:"
    log <- trim <$> hGetLine h
-- getHttpHeader ["Server:", "X-Powered-By:", "X-Runtime", "X-Version", "X-AspNet-Version"] hGetLine h
    hClose h
    return $ Just (Record host port log)

withDefault :: a -> IO a -> IO a
withDefault def action =
    handle (\(e::SomeException) -> return def) action

trim :: String -> String
trim = unpack . strip . pack