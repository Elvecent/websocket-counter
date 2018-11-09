{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Host
    ( Connection (..)
    , runHost
    , conSendData
    , conReceiveText
    ) where

import AppState
import Cmd

import qualified Web.Scotty as Sc
import qualified Data.ByteString.Lazy as BSL
import Control.Lens.TH (makeLenses)
import Control.Lens
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets as WS
import Data.Text.Lazy
import Control.Concurrent.MVar
import Control.Monad.IO.Class

data Connection =
  Connection { _conSendData :: BSL.ByteString -> IO ()
             , _conReceiveText :: IO Text
             }

makeLenses ''Connection

runHost :: (Connection -> IO ()) -> IO ()
runHost startHost = do
  let settings = Warp.setPort 8080 $
        Warp.setHost "127.0.0.1" $
        Warp.defaultSettings
  sapp <- scottyApp
  Warp.runSettings settings $
    WaiWs.websocketsOr
      WS.defaultConnectionOptions
      (wsapp startHost)
      sapp

scottyApp :: IO Wai.Application
scottyApp = Sc.scottyApp $ do
  Sc.get "/" $
    Sc.file "./html/index.html"

wsapp :: (Connection -> IO ()) -> WS.ServerApp
wsapp startHost pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  startHost Connection { _conSendData = WS.sendBinaryData conn
                       , _conReceiveText = WS.receiveData conn
                       }
