{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad  
import Data.Maybe
import Data.List
import Data.List.Split
import Network
import Network.BSD
import Network.HTTP hiding (port)
import System.Environment
import System.Exit
import System.IO
import System.Timeout
import qualified Data.Text as T
import Text.Regex.Posix

data Record = Record { ip   :: String
                     , port :: Int
                     , log  :: [String] }
   deriving Show

-- | (port, ms, scan)
scanOpts = [(22, 400, scanSSH)
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
    resp <- simpleHTTP (getRequest $ "http://" ++ host ++ ":" ++ show port)
    return $ either (const Nothing) extractHdrs resp
  where extractHdrs resp = Just (Record host port $ parseHdrs resp)
        parseHdrs   resp = map (trim . show) (filterHdrs resp)
        filterHdrs  resp = filter (\r-> hdrName r `elem` hdrs) (getHeaders resp)
        hdrs = [HdrServer,
                HdrCustom "X-Powered-By",
                HdrCustom "X-Runtime",
                HdrCustom "X-Version",
                HdrCustom "X-AspNet-Version"]

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
