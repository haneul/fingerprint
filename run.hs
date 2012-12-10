{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment
import System.Exit
import System.Cmd
import System.Locale (defaultTimeLocale)
import System.Directory
import Control.Concurrent
import Control.Monad
import Control.ThreadPool (threadPoolIO)
import Data.Time (UTCTime, getCurrentTime, formatTime)

main = do
    args <- getArgs
    case args of
      [nr] -> run (read nr)
      _    -> usage

run nr = do
  now <- getCurrentTime
  let dir = formatTime defaultTimeLocale "%F" now
  createDirectoryIfMissing True dir

  (inp, out) <- threadPoolIO nr $ loop dir
  mapM_ (writeChan inp) ips
  forM_ ips (\_ -> readChan out)
  where ips = [1..255]
        loop out ip = do
          system $ "./worker.sh " ++ show ip ++ " " ++ out
          return ()
  
usage = do
  putStrLn "[usage] #nr"
  exitFailure
  
  
