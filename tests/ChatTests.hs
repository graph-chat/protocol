{-# LANGUAGE OverloadedStrings #-}

module ChatTests where

import ChatClient
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Data.Char (isDigit)
import Data.List (dropWhileEnd, isPrefixOf)
import Simplex.Chat.Controller
import Simplex.Chat.Types (Profile (..))
import System.Terminal.Internal (VirtualTerminal (..))
import Test.Hspec

aliceProfile :: Profile
aliceProfile = Profile {displayName = "alice", fullName = "Alice"}

bobProfile :: Profile
bobProfile = Profile {displayName = "bob", fullName = "Bob"}

chatTests :: Spec
chatTests = do
  describe "direct messages" $
    it "add contact and send/receive message" testAddContact
  describe "chat groups" $
    it "add contacts, create group and send/receive messages" testGroup

testAddContact :: IO ()
testAddContact =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      alice ##> "/a"
      Just inv <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv)
      concurrently_
        (bob <## "alice (Alice) is connected")
        (alice <## "bob (Bob) is connected")
      alice #> "@bob hello"
      bob <# "alice> hello"
      bob #> "@alice hi"
      alice <# "bob> hi"
      -- test adding the same contact one more time - local name will be different
      alice ##> "/a"
      Just inv' <- invitation <$> getWindow alice
      bob ##> ("/c " <> inv')
      concurrently_
        (bob <## "alice_1 (Alice) is connected")
        (alice <## "bob_1 (Bob) is connected")
      alice #> "@bob_1 hello"
      bob <# "alice_1> hello"
      bob #> "@alice_1 hi"
      alice <# "bob_1> hi"
      -- test deleting contact
      alice ##> "/d bob_1"
      alice <## "bob_1 is deleted"
      alice #:> "@bob_1 hey"
      alice <## "no contact bob_1"

testGroup :: IO ()
testGroup =
  testChat2 aliceProfile bobProfile $
    \alice bob -> do
      connectUsers alice bob
      alice #:> "/g #team"
      -- TODO this occasionally fails in case getWindow is run before the command above is printed
      alice <## "use /a #team <name> to add members"
      alice ##> "/a #team bob admin"
      alice <## "invitation to join the group #team sent to bob"
      bob <## "use /j #team to accept"
      bob ##> "/j #team"
      concurrently_
        (alice <## "bob joined the group #team")
        (bob <## "you joined the group #team")

connectUsers :: TestCC -> TestCC -> IO ()
connectUsers cc1 cc2 = do
  cc1 ##> "/a"
  Just inv <- invitation <$> getWindow cc1
  cc2 ##> ("/c " <> inv)
  concurrently_
    (cc2 <## "alice (Alice) is connected")
    (cc1 <## "bob (Bob) is connected")

(##>) :: TestCC -> String -> IO ()
(##>) cc cmd = do
  cc #:> cmd
  cc <## cmd

(#>) :: TestCC -> String -> IO ()
(#>) cc cmd = do
  cc #:> cmd
  cc <# cmd

(#:>) :: TestCC -> String -> IO ()
(#:>) (TestCC cc _ _) cmd = atomically $ writeTBQueue (inputQ cc) $ InputCommand cmd

(<##) :: TestCC -> String -> Expectation
cc <## line = (lastOutput <$> getWindow cc) `shouldReturn` line

(<#) :: TestCC -> String -> Expectation
cc <# line = (dropTime . lastOutput <$> getWindow cc) `shouldReturn` line

dropTime :: String -> String
dropTime msg = case splitAt 6 msg of
  ([m, m', ':', s, s', ' '], text) ->
    if all isDigit [m, m', s, s'] then text else error "invalid time"
  _ -> error "invalid time"

getWindow :: TestCC -> IO [String]
getWindow (TestCC _ t _) = do
  let w = virtualWindow t
  win <- readTVarIO w
  atomically $ do
    win' <- readTVar w
    if win' /= win then pure win' else retry

invitation :: [String] -> Maybe String
invitation win = lastMaybe $ map (dropWhileEnd (== ' ')) $ filter ("smp::" `isPrefixOf`) win

lastOutput :: [String] -> String
lastOutput win = dropWhileEnd (== ' ') $ win !! (length win - 2) -- (- 2) to exclude prompt

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs
