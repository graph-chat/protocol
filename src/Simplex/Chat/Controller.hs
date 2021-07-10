{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Controller where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Simplex.Chat.Notification
import Simplex.Chat.Store (StoreError)
import Simplex.Chat.Terminal
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Protocol (AgentErrorType)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import UnliftIO.STM

data ChatController = ChatController
  { currentUser :: User,
    smpAgent :: AgentClient,
    chatTerminal :: ChatTerminal,
    chatStore :: SQLiteStore,
    inputQ :: TBQueue InputEvent,
    notifyQ :: TBQueue Notification,
    sendNotification :: Notification -> IO ()
  }

data InputEvent = InputCommand String | InputControl Char

data ChatError
  = ChatErrorContact ContactError
  | ChatErrorGroup GroupError
  | ChatErrorMessage String
  | ChatErrorAgent AgentErrorType
  | ChatErrorStore StoreError
  deriving (Show, Exception)

newtype ContactError = CENotFound ContactRef
  deriving (Show, Exception)

data GroupError = GEDuplicateGroup
  deriving (Show, Exception)

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks (activeTo . chatTerminal) >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks (activeTo . chatTerminal) >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'