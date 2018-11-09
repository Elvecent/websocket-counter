{-# LANGUAGE OverloadedStrings #-}

module Cmd
  ( Command (..)
  , parseCmd
  , evalCmd
  )
  where

import AppState

import qualified Data.Text.Lazy as T
import Control.Monad.State

data Command =
    IncCounter
  | DecCounter
  | ReqCounter
  | ErrCmd T.Text
  deriving Show

parseCmd :: T.Text -> Command
parseCmd text = case T.words text of
  ["increment", "counter"] -> IncCounter
  ["decrement", "counter"] -> DecCounter
  ["request", "counter"] -> ReqCounter
  _ -> ErrCmd text

evalCmd :: (MonadCounter m) => Command -> m (Either () T.Text)
evalCmd IncCounter = do
  update 1
  pure $ Left ()
evalCmd DecCounter = do
  update (-1)
  pure . Left $ ()
evalCmd ReqCounter = do
  c <- request
  pure . Right . T.pack . show $ c
evalCmd (ErrCmd s) = do
  let quote t = T.cons '\"' $ T.snoc t '\"'
  let error =
        "Error: received unrecognised command "
        `T.append`
        quote s
  handleError error
  pure . Right $ error
