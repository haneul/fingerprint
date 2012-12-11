{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment
import System.Exit
import System.Cmd
import System.Locale (defaultTimeLocale)
import System.Directory
import Control.Concurrent
import Control.Monad
import Data.Time (UTCTime, getCurrentTime, formatTime, diffUTCTime)
import Data.List

main = do
    args <- getArgs
    case args of
      [nproc, nsock, ip] -> run (read nproc::Int) (read nsock::Int) ip
      _ -> usage

run nproc nsock ip = do
  beg <- getCurrentTime
  let dir = formatTime defaultTimeLocale "%F" beg
  createDirectoryIfMissing True dir

  putStrLn $ "Start: " ++ show beg
  done <- newEmptyMVar
  forM [1..nproc] (forkIO . loop done ip)
  forM [1..nproc] (\_-> takeMVar done)

  end <- getCurrentTime
  let diff = diffUTCTime end beg
  putStrLn $ "End: " ++ show end ++ " @" ++ show diff
  putStrLn $ "Scanned: " ++ show (nproc * 255 * 255) ++ " = "
  return ()
  where loop done ip id = do
          let from = ip ++ "." ++ show id ++ ".0.0"
              to   = ip ++ "." ++ show id ++ ".255.255"
          putStrLn $ "[" ++ show id ++ "] scan " ++ from ++ "~" ++ to
          system $ intercalate " " ["./dist/build/scan/scan"
                                   , show nsock
                                   , from
                                   , to]
          putMVar done True
          return ()
  
usage = do
  putStrLn "[usage] #nproc #nsock ip"
  putStrLn " ex) ./run 5 1000 143"
  exitFailure
