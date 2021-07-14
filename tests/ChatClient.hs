{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatClient where

import Control.Concurrent.Async
import Control.Concurrent.STM (retry)
import Control.Monad.Except
import Simplex.Chat
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types (Profile)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal, VirtualTerminalSettings (..), withVirtualTerminal)

testDB1 :: FilePath
testDB1 = "tests/tmp/test1"

testDB2 :: FilePath
testDB2 = "tests/tmp/test2"

opts :: ChatOpts
opts =
  ChatOpts
    { dbFile = undefined,
      smpServers = ["localhost:5223"]
    }

termSettings :: VirtualTerminalSettings
termSettings =
  VirtualTerminalSettings
    { virtualType = "xterm",
      virtualWindowSize = pure C.Size {height = 24, width = 1000},
      virtualEvent = retry,
      virtualInterrupt = retry
    }

data TestCC = TestCC ChatController VirtualTerminal (Async ())

virtualSimplexChat :: FilePath -> Profile -> IO TestCC
virtualSimplexChat dbFile profile = do
  st <- createStore (dbFile <> ".chat.db") 1
  void . runExceptT $ createUser st profile True
  t <- withVirtualTerminal termSettings pure
  cc <- newChatController opts {dbFile} t . const $ pure () -- no notifications
  a <- async $ runSimplexChat cc
  pure (TestCC cc t a)

testChat2 :: Profile -> Profile -> (TestCC -> TestCC -> IO ()) -> IO ()
testChat2 p1 p2 test = do
  createDirectoryIfMissing False "tests/tmp"
  tc1 <- virtualSimplexChat testDB1 p1
  tc2 <- virtualSimplexChat testDB2 p2
  test tc1 tc2
  removeDirectoryRecursive "tests/tmp"
