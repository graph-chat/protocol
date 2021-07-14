{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat where

import Control.Applicative ((<|>))
import Control.Logger.Simple
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (drgNew)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Controller
import Simplex.Chat.Help
import Simplex.Chat.Input
import Simplex.Chat.Notification
import Simplex.Chat.Options (ChatOpts (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Store
import Simplex.Chat.Styled (plain)
import Simplex.Chat.Terminal
import Simplex.Chat.Types
import Simplex.Chat.View
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..))
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async (race_)
import UnliftIO.STM

data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddContact
  | Connect SMPQueueInfo
  | DeleteContact ContactName
  | SendMessage ContactName ByteString
  | NewGroup GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | RemoveMember GroupName ContactName
  | MemberRole GroupName ContactName GroupMemberRole
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ListMembers GroupName
  | SendGroupMessage GroupName ByteString
  deriving (Show)

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = undefined, -- filled in from options
      dbPoolSize = 4,
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

simplexChat :: WithTerminal t => ChatOpts -> t -> IO ()
simplexChat opts t = do
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  initializeNotifications
    >>= newChatController opts t
    >>= runSimplexChat

newChatController :: WithTerminal t => ChatOpts -> t -> (Notification -> IO ()) -> IO ChatController
newChatController ChatOpts {dbFile, smpServers} t sendNotification = do
  chatStore <- createStore (dbFile <> ".chat.db") 4
  currentUser <- getCreateActiveUser chatStore
  chatTerminal <- newChatTerminal t
  smpAgent <- getSMPAgentClient cfg {dbFile = dbFile <> ".agent.db", smpServers}
  idsDrg <- newTVarIO =<< drgNew
  inputQ <- newTBQueueIO $ tbqSize cfg
  notifyQ <- newTBQueueIO $ tbqSize cfg
  pure ChatController {currentUser, smpAgent, chatTerminal, chatStore, idsDrg, inputQ, notifyQ, sendNotification}

runSimplexChat :: ChatController -> IO ()
runSimplexChat = runReaderT (race_ runTerminalInput runChatController)

runChatController :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
runChatController =
  raceAny_
    [ inputSubscriber,
      agentSubscriber,
      notificationSubscriber
    ]

inputSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
inputSubscriber = do
  q <- asks inputQ
  forever $
    atomically (readTBQueue q) >>= \case
      InputControl _ -> pure ()
      InputCommand s ->
        case parseAll chatCommandP . encodeUtf8 $ T.pack s of
          Left e -> printToView [plain s, "invalid input: " <> plain e]
          Right cmd -> do
            case cmd of
              SendMessage c msg -> showSentMessage c msg
              _ -> printToView [plain s]
            user <- asks currentUser
            void . runExceptT $ processChatCommand user cmd `catchError` showChatError

processChatCommand :: ChatMonad m => User -> ChatCommand -> m ()
processChatCommand user@User {userId, profile} = \case
  ChatHelp -> printToView chatHelpInfo
  MarkdownHelp -> printToView markdownInfo
  AddContact -> do
    (connId, qInfo) <- withAgent createConnection
    withStore $ \st -> createDirectConnection st userId connId
    showInvitation qInfo
  Connect qInfo -> do
    connId <- withAgent $ \a -> joinConnection a qInfo . directMessage $ XInfo profile
    withStore $ \st -> createDirectConnection st userId connId
  DeleteContact cName -> do
    conns <- withStore $ \st -> getContactConnections st userId cName
    withAgent $ \a -> forM_ conns $ \Connection {agentConnId} ->
      deleteConnection a agentConnId `catchError` \(_ :: AgentErrorType) -> pure ()
    withStore $ \st -> deleteContact st userId cName
    unsetActive $ ActiveC cName
    showContactDeleted cName
  SendMessage cName msg -> do
    contact <- withStore $ \st -> getContact st userId cName
    let msgEvent = XMsgNew MTText [] [MsgBodyContent {contentType = SimplexContentType XCText, contentData = msg}]
    sendDirectMessage (contactConnId contact) msgEvent
    setActive $ ActiveC cName
  NewGroup gProfile -> do
    gVar <- asks idsDrg
    group <- withStore $ \st -> createNewGroup st gVar user gProfile
    showGroupCreated group
  AddMember gName cName memRole -> do
    (group, contact) <- withStore $ \st -> (,) <$> getGroup st user gName <*> getContact st userId cName
    let Group {groupId, groupProfile, membership, members} = group
        userRole = memberRole membership
        userMemberId = memberId membership
    when (userRole < GRAdmin || userRole < memRole) $ throwError $ ChatError CEGroupRole
    when (isMember contact members) . throwError . ChatError $ CEGroupDuplicateMember cName
    when (memberStatus membership == GSMemInvited) . throwError . ChatError $ CEGroupNotJoined gName
    when (memberStatus membership < GSMemReady) . throwError . ChatError $ CEGroupMemberNotReady
    gVar <- asks idsDrg
    (agentConnId, qInfo) <- withAgent createConnection
    GroupMember {memberId} <- withStore $ \st -> createGroupMember st gVar user groupId contact memRole agentConnId
    let msg = XGrpInv $ GroupInvitation (userMemberId, userRole) (memberId, memRole) qInfo groupProfile
    sendDirectMessage (contactConnId contact) msg
    showSentGroupInvitation group cName
  JoinGroup gName -> do
    ReceivedGroupInvitation {fromMember, invitedMember, queueInfo} <- withStore $ \st -> getGroupInvitation st user gName
    agentConnId <- withAgent $ \a -> joinConnection a queueInfo . directMessage . XGrpAcpt $ memberId invitedMember
    withStore $ \st -> createMemberConnection st userId (groupMemberId fromMember) agentConnId
  MemberRole _gRef _cRef _mRole -> pure ()
  RemoveMember _gRef _cRef -> pure ()
  LeaveGroup _gRef -> pure ()
  DeleteGroup _gRef -> pure ()
  ListMembers _gRef -> pure ()
  SendGroupMessage _gRef _msg -> pure ()
  where
    isMember :: Contact -> [(GroupMember, Maybe Connection)] -> Bool
    isMember Contact {contactId} members = isJust $ find ((== Just contactId) . memberContactId . fst) members

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  q <- asks $ subQ . smpAgent
  forever $ do
    (_, connId, msg) <- atomically $ readTBQueue q
    user <- asks currentUser
    -- TODO handle errors properly
    void . runExceptT $ processAgentMessage user connId msg `catchError` (liftIO . print)

processAgentMessage :: forall m. ChatMonad m => User -> ConnId -> ACommand 'Agent -> m ()
processAgentMessage user@User {userId, profile} agentConnId agentMessage = do
  chatDirection <- withStore $ \st -> getConnectionChatDirection st user agentConnId
  case chatDirection of
    ReceivedDirectMessage (CContact ct@Contact {localDisplayName = c}) ->
      case agentMessage of
        MSG meta msgBody -> do
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage msgBody
          case chatMsgEvent of
            XMsgNew MTText [] body -> newTextMessage c meta $ find (isSimplexContentType XCText) body
            XInfo _ -> pure () -- TODO profile update
            XGrpInv gInv -> saveGroupInvitation ct gInv
            _ -> pure ()
        CON -> do
          -- TODO update connection status
          showContactConnected ct
          showToast ("@" <> c) "connected"
          setActive $ ActiveC c
        END -> do
          showContactDisconnected c
          showToast ("@" <> c) "disconnected"
          unsetActive $ ActiveC c
        _ -> pure ()
    ReceivedDirectMessage (CConnection conn) ->
      case agentMessage of
        CONF confId connInfo -> do
          -- TODO update connection status
          saveConnInfo conn connInfo
          withAgent $ \a -> allowConnection a agentConnId confId . directMessage $ XInfo profile
        INFO connInfo ->
          saveConnInfo conn connInfo
        _ -> pure ()
    ReceivedGroupMessage gName m ->
      case agentMessage of
        CONF confId connInfo -> do
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
          case chatMsgEvent of
            XGrpAcpt memId
              | memId == memberId m -> do
                withStore $ \st -> updateGroupMemberStatus st userId (groupMemberId m) GSMemAccepted
                withAgent $ \a -> allowConnection a agentConnId confId ""
              | otherwise -> pure () -- TODO error not matching member ID
            _ -> pure () -- TODO show/log error, other events in SMP confirmation
        CON -> do
          Group {membership, members} <- withStore $ \st -> getGroup st user gName
          -- TODO because the contact is not created instantly, if the member is not created from contact,
          -- it should still have a unique local display name.
          -- If it is created from contact it can still be duplicated on the member (and match the contact)
          case invitedBy m of
            IBUser -> do
              -- sender was invited by the current user
              withStore $ \st -> updateGroupMemberStatus st userId (groupMemberId m) GSMemConnected
              sendGroupMessage members $ XGrpMemNew (memberId m) (memberRole m) (memberProfile m)
              showConnectedGroupMember gName $ displayName (memberProfile m :: Profile)
              forM_ (filter (\m' -> memberStatus m' >= GSMemConnected) . map fst $ members) $ \m' ->
                sendDirectMessage agentConnId $ XGrpMemIntro (memberId m') (memberRole m') (memberProfile m')
            _ -> do
              if Just (invitedBy membership) == (IBContact <$> memberContactId m)
                then do
                  -- sender invited the current user
                  withStore $ \st -> updateGroupMemberStatus st userId (groupMemberId m) GSMemConnected
                  showUserConnectedToGroup gName
                  pure ()
                else do
                  showConnectedGroupMember gName $ displayName (memberProfile m :: Profile)
        MSG meta msgBody -> do
          ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage msgBody
          case chatMsgEvent of
            XGrpMemNew memId memRole memProfile -> do
              Group {membership, members} <- withStore $ \st -> getGroup st user gName
              when (memberId membership /= memId && isNothing (find ((== memId) . memberId . fst) members)) $
                withStore $ \st -> pure () -- add new member as GSMemAccepted
            _ -> pure ()
        _ -> pure ()
  where
    newTextMessage :: ContactName -> MsgMeta -> Maybe MsgBodyContent -> m ()
    newTextMessage c meta = \case
      Just MsgBodyContent {contentData = bs} -> do
        let text = safeDecodeUtf8 bs
        showReceivedMessage c (snd $ broker meta) text (integrity meta)
        showToast ("@" <> c) text
        setActive $ ActiveC c
      _ -> pure ()

    saveGroupInvitation :: Contact -> GroupInvitation -> m ()
    saveGroupInvitation ct@Contact {localDisplayName} inv@(GroupInvitation (fromMemId, fromRole) (memId, memRole) _ _) = do
      when (fromRole < GRAdmin || fromRole < memRole) $ throwError $ ChatError CEGroupRole
      when (fromMemId == memId) $ throwError $ ChatError CEGroupDuplicateMemberId
      group <- withStore $ \st -> createGroupInvitation st user ct inv
      showReceivedGroupInvitation group localDisplayName

    parseChatMessage :: ByteString -> Either ChatError ChatMessage
    parseChatMessage msgBody = first ChatErrorMessage (parseAll rawChatMessageP msgBody >>= toChatMessage)

    saveConnInfo :: Connection -> ConnInfo -> m ()
    saveConnInfo activeConn connInfo = do
      ChatMessage {chatMsgEvent} <- liftEither $ parseChatMessage connInfo
      case chatMsgEvent of
        XInfo p ->
          withStore $ \st -> createDirectContact st userId activeConn p
        _ -> pure () -- TODO show/log error, other events in SMP confirmation

sendDirectMessage :: ChatMonad m => ConnId -> ChatMsgEvent -> m ()
sendDirectMessage agentConnId chatMsgEvent =
  void . withAgent $ \a -> sendMessage a agentConnId $ directMessage chatMsgEvent

directMessage :: ChatMsgEvent -> ByteString
directMessage chatMsgEvent =
  serializeRawChatMessage $
    rawChatMessage ChatMessage {chatMsgId = Nothing, chatMsgEvent, chatDAG = Nothing}

sendGroupMessage :: ChatMonad m => [(GroupMember, Maybe Connection)] -> ChatMsgEvent -> m ()
sendGroupMessage _members _chatMsgEvent = pure ()

getCreateActiveUser :: SQLiteStore -> IO User
getCreateActiveUser st = do
  user <-
    getUsers st >>= \case
      [] -> newUser
      users -> maybe (selectUser users) pure (find activeUser users)
  putStrLn $ "Current user: " <> userStr user
  pure user
  where
    newUser :: IO User
    newUser = do
      putStrLn
        "No user profiles found, it will be created now.\n\
        \Please choose your display name and your full name.\n\
        \They will be sent to your contacts when you connect.\n\
        \They are only stored on your device and you can change them later."
      loop
      where
        loop = do
          displayName <- getContactName
          fullName <- T.pack <$> getWithPrompt "full name (optional)"
          liftIO (runExceptT $ createUser st Profile {displayName, fullName} True) >>= \case
            Left SEDuplicateName -> do
              putStrLn "chosen display name is already used by another profile on this device, choose another one"
              loop
            Left e -> putStrLn ("database error " <> show e) >> exitFailure
            Right user -> pure user
    selectUser :: [User] -> IO User
    selectUser [user] = do
      liftIO $ setActiveUser st (userId user)
      pure user
    selectUser users = do
      putStrLn "Select user profile:"
      forM_ (zip [1 ..] users) $ \(n :: Int, user) -> putStrLn $ show n <> " - " <> userStr user
      loop
      where
        loop = do
          nStr <- getWithPrompt $ "user profile number (1 .. " <> show (length users) <> ")"
          case readMaybe nStr :: Maybe Int of
            Nothing -> putStrLn "invalid user number" >> loop
            Just n
              | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
              | otherwise -> do
                let user = users !! (n - 1)
                liftIO $ setActiveUser st (userId user)
                pure user
    userStr :: User -> String
    userStr User {localDisplayName, profile = Profile {fullName}} =
      T.unpack $ localDisplayName <> if T.null fullName then "" else " (" <> fullName <> ")"
    getContactName :: IO ContactName
    getContactName = do
      displayName <- getWithPrompt "display name (no spaces)"
      if null displayName || isJust (find (== ' ') displayName)
        then putStrLn "display name has space(s), choose another one" >> getContactName
        else pure $ T.pack displayName
    getWithPrompt :: String -> IO String
    getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

showToast :: (MonadUnliftIO m, MonadReader ChatController m) => Text -> Text -> m ()
showToast title text = atomically . (`writeTBQueue` Notification {title, text}) =<< asks notifyQ

notificationSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

withAgent :: ChatMonad m => (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first ChatErrorAgent

withStore ::
  ChatMonad m =>
  (forall m'. (MonadUnliftIO m', MonadError StoreError m') => SQLiteStore -> m' a) ->
  m a
withStore action =
  asks chatStore
    >>= runExceptT . action
    >>= liftEither . first ChatErrorStore

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/group #" <|> "/g #") *> (NewGroup <$> groupProfile)
    <|> ("/add #" <|> "/a #") *> (AddMember <$> displayName <* A.space <*> displayName <*> memberRole)
    <|> ("/join #" <|> "/j #") *> (JoinGroup <$> displayName)
    <|> ("/remove #" <|> "/rm #") *> (RemoveMember <$> displayName <* A.space <*> displayName)
    <|> ("/delete #" <|> "/d #") *> (DeleteGroup <$> displayName)
    <|> ("/members #" <|> "/ms #") *> (ListMembers <$> displayName)
    <|> A.char '#' *> (SendGroupMessage <$> displayName <* A.space <*> A.takeByteString)
    <|> ("/add" <|> "/a") $> AddContact
    <|> ("/connect " <|> "/c ") *> (Connect <$> smpQueueInfoP)
    <|> ("/delete @" <|> "/delete " <|> "/d @" <|> "/d ") *> (DeleteContact <$> displayName)
    <|> A.char '@' *> (SendMessage <$> displayName <*> (A.space *> A.takeByteString))
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    displayName = safeDecodeUtf8 <$> (B.cons <$> A.satisfy refChar <*> A.takeTill (== ' '))
    refChar c = c > ' ' && c /= '#' && c /= '@'
    groupProfile = do
      gName <- displayName
      fullName' <- safeDecodeUtf8 <$> (A.space *> A.takeByteString) <|> pure ""
      pure GroupProfile {displayName = gName, fullName = if T.null fullName' then gName else fullName'}
    memberRole =
      (" owner" $> GROwner)
        <|> (" admin" $> GRAdmin)
        <|> (" normal" $> GRMember)
        <|> pure GRMember
