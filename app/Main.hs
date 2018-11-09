module Main where

import Host
import AppState (runCounter, initialState)
import Cmd

import Control.Lens ((^.))
import Data.Text.Lazy.Encoding (encodeUtf8)
import Control.Monad (forever)
import Control.Monad.State (runStateT)
import Control.Concurrent.MVar
import Data.Text.Lazy (Text)

main :: IO ()
main = runHost manageConnection

manageConnection :: Connection -> IO ()
manageConnection conn = do
  var <- newMVar initialState
  forever $ do
    text <- conn^.conReceiveText
    let cmd = runCounter . evalCmd . parseCmd $ text
    state <- takeMVar var
    (res, state') <- runStateT cmd state
    putMVar var state'
    either
      pure
      ((conn^.conSendData) . encodeUtf8)
      (res :: Either () Text)
