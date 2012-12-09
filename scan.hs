{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad  
import Data.Maybe
import Data.List
import Data.List.Split
import Data.List.Utils
import Network
import Network.BSD
import Network.HTTP hiding (port)
import System.Environment
import System.Exit
import System.IO
import System.Timeout
import qualified Data.Text as T

data Record = Record { ip   :: String
                     , port :: Int
                     , log  :: [String] }
   deriving Show

-- | (port, ms, scan)
scanOpts = [(22, 600, scanSSH)
           ,(80, 800, scanHTTP)]

main :: IO ()
main = do
    args <- getArgs
    case args of
      [host] -> withSocketsDo $ doScan (parseHost host)
      _      -> usage

usage = do
  putStrLn "[usage] ip"
  exitFailure
  
doScan :: [String] -> IO ()
doScan hosts = do
    forM_ scanOpts $ \(port, ms, scan) ->
      mapM (initScan port ms scan) hosts >>= mapM_ hitCheck
  where
    initScan port ms scan host = threadWithChannel (withDefault Nothing $ scan host port) ms
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
    return $ Just (Record host port [log])

scanHTTP :: String -> Int -> IO (Maybe Record)
scanHTTP host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hPutStr h "GET / HTTP/1.1\n\
              \User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0\n\
              \Host: xk\n\
              \Accept: */*\n\n"
    -- lookup only 20 headers
    resp <- replicateM 20 $ hGetLine h
    -- parse headers
    log <- return $ parseHdrs (drop 1 resp)
    hClose h
    return $ Just (Record host port log)
  where parseHdrs (line:resp) | checkHdrs hdrs line  = line:(parseHdrs resp)
                              | ':' `elem` line      = parseHdrs resp
                              | otherwise            = []
        checkHdrs [] _ = False
        checkHdrs (x:xs) line | startswith x line = True
                              | otherwise = checkHdrs xs line
        hdrs = ["Server:"
               ,"X-Powered-By:"
               ,"X-Runtime:"
               ,"X-Version:"
               ,"X-AspNet-Version:"]

withDefault :: a -> IO a -> IO a
withDefault def action =
    handle (\(e::SomeException) -> return def) action

trim :: String -> String
trim = T.unpack . T.strip . T.pack

parseHost :: String -> [String]
parseHost host = map (intercalate ".") $ expand (splitOn "." host)
  where expand [] = [[]]
        expand (x:xs) | x == "*"  = [x:y| x <- map show [0..255], y <- expand xs]
                      | otherwise = [x:y| y <- expand xs]
