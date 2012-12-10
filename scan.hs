{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad  
import Data.Maybe
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Time (UTCTime, getCurrentTime, formatTime)
import Network
import Network.BSD
import Network.HTTP hiding (port)
import System.Environment
import System.Exit
import System.IO
import System.IO.Utils
import System.Locale (defaultTimeLocale)
import System.Timeout
import qualified Data.Text as T
import Text.Printf

data Record = Record { time :: UTCTime
                     , ip   :: String
                     , port :: Int
                     , logs :: [String] }
    deriving Show

mkRecord ip port logs = do
    now <- getCurrentTime
    return $ Just (Record now ip port logs)

logRecord h (Record time ip port logs) =
    hPrintf h "%s %-15s %-3d %s\n" fmt ip port (intercalate "," $ logs)
  where fmt = formatTime defaultTimeLocale "%F %T" time

-- | (port, ms, scan)
scanOpts = [(22, 3000, scanSSH)
           ,(80, 5000, scanHTTP)]

main :: IO ()
main = do
    args <- getArgs
    case args of
      [host] -> withSocketsDo $ doScan host
      _      -> usage

usage = do
  putStrLn "[usage] ip"
  exitFailure
  
doScan :: String -> IO ()
doScan host = do
    forM_ scanOpts $ \(port, ms, scan) ->
      mapM (initScan port ms scan) (parseHost host) >>= mapM_ hitCheck
  where
    initScan port ms scan host = forkWithChnl (withDef Nothing $ scan host port) ms
    hitCheck mvar = takeMVar mvar >>= maybe (return ()) printHit
    printHit = logRecord stdout

forkWithChnl :: IO (Maybe Record) -> Int -> IO (MVar (Maybe Record))
forkWithChnl action ms = do
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
    mkRecord host port [log]

getLines :: Int -> Handle -> IO [String]
getLines 0 h = return $ []
getLines n h = do
    eof <- hIsEOF h
    if eof
      then do
        return []
      else do
        line <- hGetLine h
        rest <- getLines (n-1) h
        return $! line:rest

scanHTTP :: String -> Int -> IO (Maybe Record)
scanHTTP host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hPutStr h $ "GET / HTTP/1.1\r\n\
                \User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0\r\n\
                \Host: " ++ host ++ "\r\n\
                \Accept: */*\r\n\r\n"
    -- lookup only 20 lines at maximum
    resp <- getLines 20 h
    -- parse headers
    logs <- return $ parseHdrs (drop 1 resp)
    hClose h
    mkRecord host port logs
  where parseHdrs [] = []
        parseHdrs (line:resp) | checkHdrs hdrs line  = (trim line):(parseHdrs resp)
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

withDef :: a -> IO a -> IO a
withDef def action =
    handle (\(e::SomeException) -> return def) action

trim :: String -> String
trim = T.unpack . T.strip . T.pack

parseHost :: String -> [String]
parseHost host = map (intercalate ".") $ expand (splitOn "." host)
  where expand [] = [[]]
        expand (x:xs) | x == "*"  = [x:y| x <- map show [0..255], y <- expand xs]
                      | otherwise = [x:y| y <- expand xs]
