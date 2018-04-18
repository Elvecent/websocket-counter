{-# LANGUAGE OverloadedStrings #-}

module Cmd
  ( Command (..)
  , parseCmd
  , evalCmd
  )
  where

import AppState

import qualified Data.Text.Lazy as Txt
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Lens

data Command =
    IncCounter
  | DecCounter
  | ReqCounter
  | ErrCmd Txt.Text
  deriving Show

parseCmd :: Txt.Text -> Command
parseCmd text = case Txt.words text of
  ["increment", "counter"] -> IncCounter
  ["decrement", "counter"] -> DecCounter
  ["request", "counter"] -> ReqCounter
  _ -> ErrCmd text

evalCmd :: Command -> StateIO (Either () Txt.Text)
evalCmd IncCounter = do
  counter += 1
  return . Left $ ()
evalCmd DecCounter = do
  counter -= 1
  return . Left $ ()
evalCmd ReqCounter = do
  state <- get
  return . Right . Txt.pack . show $
    state^.counter
evalCmd (ErrCmd s) = do
  let quote t = Txt.cons '\"' $ Txt.snoc t '\"'
  let error =
        "Error: received unrecognised command "
        `Txt.append`
        quote s
  lift $ putStrLn . Txt.unpack $ error
  return . Right $ error
