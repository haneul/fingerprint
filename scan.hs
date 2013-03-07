{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad  
import Data.Maybe
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.IP
import Data.Time (UTCTime, getCurrentTime, formatTime)
import Network
import Network.BSD
import Network.HTTP hiding (port)
import System.Environment
import System.Exit
import System.IO
import System.Locale (defaultTimeLocale)
import System.Timeout
import qualified Data.Text as T
import Text.Printf
import Text.Regex.Posix

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
scanOpts = [(22,  600*5, scanSSH)
           ,(80, 1000*5, scanHTTP)]
           -- ,(80, 1000*5, scanWPReadme)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [nc, from, to] -> initScan (read nc) (read from) (read to)
      _              -> usage

initScan :: Int -> IPv4 -> IPv4 -> IO ()
initScan nc from to | from <= to = withSocketsDo $ doScan nc from to
                    | otherwise  = usage               

nextIP :: IPv4 -> IPv4
nextIP ip = case newIp of
    [ 10,  _,_,_] -> toIPv4 [ 11,  0,0,0]
    [172, 16,_,_] -> toIPv4 [172, 32,0,0]
    [169,254,_,_] -> toIPv4 [169,255,0,0]
    [192,168,_,_] -> toIPv4 [192,169,0,0]
    otherwise     -> toIPv4 newIp
  where tok   = fromIPv4 ip
        newIp = reverse . incIP . reverse $ tok
        incIP []       = []
        incIP (255:xs) = 0:(incIP xs)
        incIP (x:xs)   = (x+1):xs

usage :: IO ()
usage = do
    putStrLn "[usage] #connections from to"
    exitFailure

doScan :: Int -> IPv4 -> IPv4 -> IO ()
doScan nc from to = do
    done  <- newEmptyMVar
    hosts <- newMVar from
    forM [1..nc] $ \id -> forkIO (worker id hosts done)
    forM [1..nc] $ \id -> takeMVar done
    return ()
  where worker id hosts done = do
          host <- takeMVar hosts
          let next = nextIP host
          putMVar hosts next
          if next > to then do
            putMVar done True
          else do
            forM_ scanOpts $ \(port, ms, scan) ->
              loop port ms scan host >>= check
            worker id hosts done
          
        loop port ms scan host = do
          let action = withDef Nothing $ scan (show host) port
          timeout (ms*1000) action >>= maybe (return Nothing) (return)

        check Nothing  = return ()
        check (Just r) = logRecord stdout r

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

httpGet url host port = do
    h <- connectTo host (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hPutStr h $ "GET " ++ url ++ " HTTP/1.1\r\n\
                \User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0\r\n\
                \Host: " ++ host ++ "\r\n\
                \Accept: */*\r\n\r\n"
    return h

scanWPReadme :: String -> Int -> IO (Maybe Record)
scanWPReadme host port = do
    h <- httpGet "/readme.html" host port
    resp <- hGetContents h
    hClose h
    case resp =~~ "Version ([0-9]+.[0-9]+.[0-9]+)" :: Maybe String of
      Just ver -> mkRecord host port [ver]
      Nothing -> return Nothing

scanHTTP :: String -> Int -> IO (Maybe Record)
scanHTTP host port = do
    h <- httpGet "/" host port
    -- lookup only 20 lines at most
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