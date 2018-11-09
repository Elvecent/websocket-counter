{-# LANGUAGE OverloadedStrings,
             TemplateHaskell,
             GeneralizedNewtypeDeriving
#-}

module AppState
  ( AppState
  , App
  , counter
  , initialState
  , MonadCounter (..)
  , runApp
  ) where

import Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Control.Lens.TH (makeLenses)
import Control.Lens ((+=), (^.))
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

data AppState = AppState
  { _counter :: Int
  } deriving Show

makeLenses ''AppState

newtype App a = App {runApp :: StateT AppState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState AppState)

initialState :: AppState
initialState = AppState
  { _counter = 0
  }

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . TIO.putStrLn


class Monad m => MonadCounter m where
  update :: Int -> m ()
  request :: m Int
  handleError :: T.Text -> m ()

instance MonadCounter App where
  update n = do
    putTextLn $
      "Counter modification: "
      `T.append`
      (T.pack $ show n)
    counter += n

  request = do
    putTextLn "Counter value requested"
    state <- get
    pure $ state ^. counter

  handleError e = putTextLn e
