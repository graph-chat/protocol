{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.View
  ( printToView,
    showInvitation,
    showChatError,
    showContactDeleted,
    showContactConnected,
    showContactDisconnected,
    showReceivedMessage,
    showSentMessage,
    showGroupCreated,
    showSentGroupInvitation,
    showReceivedGroupInvitation,
    showConnectedGroupMember,
    showUserConnectedToGroup,
    safeDecodeUtf8,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Composition ((.:))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, getZonedTime, localDay, localTimeOfDay, timeOfDayToTime, utcToLocalTime, zonedTimeToLocalTime)
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Store (StoreError (..))
import Simplex.Chat.Styled
import Simplex.Chat.Terminal (printToTerminal)
import Simplex.Chat.Types
import Simplex.Chat.Util (safeDecodeUtf8)
import Simplex.Messaging.Agent.Protocol
import System.Console.ANSI.Types

type ChatReader m = (MonadUnliftIO m, MonadReader ChatController m)

showInvitation :: ChatReader m => SMPQueueInfo -> m ()
showInvitation = printToView . invitation

showChatError :: ChatReader m => ChatError -> m ()
showChatError = printToView . chatError

showContactDeleted :: ChatReader m => ContactName -> m ()
showContactDeleted = printToView . contactDeleted

showContactConnected :: ChatReader m => Contact -> m ()
showContactConnected = printToView . contactConnected

showContactDisconnected :: ChatReader m => ContactName -> m ()
showContactDisconnected = printToView . contactDisconnected

showReceivedMessage :: ChatReader m => ContactName -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedMessage c utcTime msg mOk = printToView =<< liftIO (receivedMessage c utcTime msg mOk)

showSentMessage :: ChatReader m => ContactName -> ByteString -> m ()
showSentMessage c msg = printToView =<< liftIO (sentMessage c msg)

showGroupCreated :: ChatReader m => Group -> m ()
showGroupCreated = printToView . groupCreated

showSentGroupInvitation :: ChatReader m => Group -> ContactName -> m ()
showSentGroupInvitation = printToView .: sentGroupInvitation

showReceivedGroupInvitation :: ChatReader m => Group -> ContactName -> m ()
showReceivedGroupInvitation = printToView .: receivedGroupInvitation

showConnectedGroupMember :: ChatReader m => GroupName -> ContactName -> m ()
showConnectedGroupMember = printToView .: connectedGroupMember

showUserConnectedToGroup :: ChatReader m => GroupName -> m ()
showUserConnectedToGroup = printToView . userConnectedToGroup

invitation :: SMPQueueInfo -> [StyledString]
invitation qInfo =
  [ "pass this invitation to your contact (via another channel): ",
    "",
    (plain . serializeSmpQueueInfo) qInfo,
    "",
    "and ask them to connect: " <> highlight' "/c <invitation_above>"
  ]

contactDeleted :: ContactName -> [StyledString]
contactDeleted c = [ttyContact c <> " is deleted"]

contactConnected :: Contact -> [StyledString]
contactConnected ct = [ttyFullContact ct <> " is connected"]

contactDisconnected :: ContactName -> [StyledString]
contactDisconnected c = ["disconnected from " <> ttyContact c <> " - restart chat"]

groupCreated :: Group -> [StyledString]
groupCreated g@Group {localDisplayName} =
  [ "group " <> ttyFullGroup g <> " is created",
    "use " <> highlight ("/a #" <> localDisplayName <> " <name>") <> " to add members"
  ]

sentGroupInvitation :: Group -> ContactName -> [StyledString]
sentGroupInvitation g c = ["invitation to join the group " <> ttyFullGroup g <> " sent to " <> ttyContact c]

receivedGroupInvitation :: Group -> ContactName -> [StyledString]
receivedGroupInvitation g@Group {localDisplayName} c =
  [ ttyContact c <> " invites you to join the group " <> ttyFullGroup g,
    "use " <> highlight ("/j #" <> localDisplayName) <> " to accept"
  ]

connectedGroupMember :: GroupName -> ContactName -> [StyledString]
connectedGroupMember g c = [ttyContact c <> " joined the group " <> ttyGroup g]

userConnectedToGroup :: GroupName -> [StyledString]
userConnectedToGroup g = ["you joined the group " <> ttyGroup g]

receivedMessage :: ContactName -> UTCTime -> Text -> MsgIntegrity -> IO [StyledString]
receivedMessage c utcTime msg mOk = do
  t <- formatUTCTime <$> getCurrentTimeZone <*> getZonedTime
  pure $ prependFirst (t <> " " <> ttyFromContact c) (msgPlain msg) ++ showIntegrity mOk
  where
    formatUTCTime :: TimeZone -> ZonedTime -> StyledString
    formatUTCTime localTz currentTime =
      let localTime = utcToLocalTime localTz utcTime
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime currentTime))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
       in styleTime $ formatTime defaultTimeLocale format localTime
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err) = msgError $ case err of
      MsgSkipped fromId toId ->
        "skipped message ID " <> show fromId
          <> if fromId == toId then "" else ".." <> show toId
      MsgBadId msgId -> "unexpected message ID " <> show msgId
      MsgBadHash -> "incorrect message hash"
      MsgDuplicate -> "duplicate message ID"
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

sentMessage :: ContactName -> ByteString -> IO [StyledString]
sentMessage c msg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> ttyToContact c) (msgPlain $ safeDecodeUtf8 msg)

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

chatError :: ChatError -> [StyledString]
chatError = \case
  ChatError err -> case err of
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupRole -> ["insufficient role for this group command"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> g)]
    CEGroupMemberNotReady -> ["you cannot invite other members yet, try later"]
    CEGroupInternal s -> ["chat group bug: " <> plain s]
  -- e -> ["chat error: " <> plain (show e)]
  ChatErrorStore err -> case err of
    SEDuplicateName -> ["this display name is already used by user, contact or group"]
    SEContactNotFound c -> ["no contact " <> ttyContact c]
    SEContactNotReady c -> ["contact " <> ttyContact c <> " is not active yet"]
    SEGroupNotFound g -> ["no group " <> ttyGroup g]
    SEGroupAlreadyJoined -> ["you already joined this group"]
    e -> ["chat db error: " <> plain (show e)]
  ChatErrorAgent err -> case err of
    -- CONN e -> case e of
    --   -- TODO replace with ChatErrorContact errors, these errors should never happen
    --   NOT_FOUND -> ["no contact " <> ttyContact c]
    --   DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    --   SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
    e -> ["smp agent error: " <> plain (show e)]
  ChatErrorMessage e -> ["chat message error: " <> plain (show e)]

printToView :: (MonadUnliftIO m, MonadReader ChatController m) => [StyledString] -> m ()
printToView s = asks chatTerminal >>= liftIO . (`printToTerminal` s)

ttyContact :: ContactName -> StyledString
ttyContact = styled (Colored Green)

ttyFullContact :: Contact -> StyledString
ttyFullContact Contact {localDisplayName, profile = Profile {fullName}} =
  ttyContact localDisplayName <> optFullName localDisplayName fullName

ttyToContact :: ContactName -> StyledString
ttyToContact c = styled (Colored Cyan) $ "@" <> c <> " "

ttyFromContact :: ContactName -> StyledString
ttyFromContact c = styled (Colored Yellow) $ c <> "> "

ttyGroup :: GroupName -> StyledString
ttyGroup g = styled (Colored Blue) $ "#" <> g

ttyFullGroup :: Group -> StyledString
ttyFullGroup Group {localDisplayName, groupProfile = GroupProfile {fullName}} =
  ttyGroup localDisplayName <> optFullName localDisplayName fullName

optFullName :: Text -> Text -> StyledString
optFullName localDisplayName fullName
  | localDisplayName == fullName = ""
  | otherwise = plain (" (" <> fullName <> ")")

highlight :: StyledFormat a => a -> StyledString
highlight = styled (Colored Cyan)

highlight' :: String -> StyledString
highlight' = highlight

-- ttyFromGroup :: Group -> Contact -> StyledString
-- ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]
