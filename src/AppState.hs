{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState
  ( AppState
  , StateIO
  , counter
  , initialState
  , modifyCounter
  , getCounter
  ) where

import Data.Text
import Control.Lens.TH (makeLenses)
import Control.Lens
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

data AppState = AppState
  { _counter :: Int
  } deriving Show

makeLenses ''AppState

type StateIO a = StateT AppState IO a

initialState :: AppState
initialState = AppState
  { _counter = 0
  }

putText :: Text -> IO ()
putText = putStrLn . unpack

modifyCounter :: Int -> StateIO ()
modifyCounter num = do
  lift . putText $
    "Counter modification: " `append` (pack $ show num)
  counter += num

getCounter :: StateIO Int
getCounter = do
  state <- get
  return $ state^.counter
