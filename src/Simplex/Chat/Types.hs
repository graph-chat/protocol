{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics
import Simplex.Messaging.Agent.Protocol (ConnId, SMPQueueInfo)
import Simplex.Messaging.Agent.Store.SQLite (fromTextField_)

class IsContact a where
  contactId' :: a -> Int64
  profile' :: a -> Profile

instance IsContact User where
  contactId' = userContactId
  profile' = profile

instance IsContact Contact where
  contactId' = contactId
  profile' = profile

data User = User
  { userId :: UserId,
    userContactId :: Int64,
    localDisplayName :: ContactName,
    profile :: Profile,
    activeUser :: Bool
  }

type UserId = Int64

data Contact = Contact
  { contactId :: Int64,
    localDisplayName :: ContactName,
    profile :: Profile,
    activeConn :: Connection
  }
  deriving (Eq, Show)

contactConnId :: Contact -> ConnId
contactConnId Contact {activeConn = Connection {agentConnId}} = agentConnId

type ContactName = Text

type GroupName = Text

data Group = Group
  { groupId :: Int64,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    members :: [(GroupMember, Maybe Connection)],
    membership :: GroupMember
  }
  deriving (Eq, Show)

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON Profile where toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON Profile

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text
  }
  deriving (Generic, Eq, Show)

instance ToJSON GroupProfile where toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON GroupProfile

data GroupInvitation = GroupInvitation
  { fromMember :: MemberInfo,
    invitedMember :: MemberInfo,
    queueInfo :: SMPQueueInfo,
    groupProfile :: GroupProfile
  }
  deriving (Eq, Show)

data ReceivedGroupInvitation = ReceivedGroupInvitation
  { fromMember :: GroupMember,
    invitedMember :: GroupMember,
    queueInfo :: SMPQueueInfo,
    groupProfile :: GroupProfile
  }
  deriving (Eq, Show)

type MemberInfo = (MemberId, GroupMemberRole)

data GroupMember = GroupMember
  { groupMemberId :: Int64,
    memberId :: MemberId,
    memberRole :: GroupMemberRole,
    memberStatus :: GroupMemberStatus,
    invitedBy :: InvitedBy,
    memberProfile :: Profile,
    memberContactId :: Maybe Int64
  }
  deriving (Eq, Show)

type MemberId = ByteString

data InvitedBy = IBContact Int64 | IBUser | IBUnknown
  deriving (Eq, Show)

toInvitedBy :: Int64 -> Maybe Int64 -> InvitedBy
toInvitedBy userCtId (Just ctId)
  | userCtId == ctId = IBUser
  | otherwise = IBContact ctId
toInvitedBy _ Nothing = IBUnknown

fromInvitedBy :: Int64 -> InvitedBy -> Maybe Int64
fromInvitedBy userCtId = \case
  IBUnknown -> Nothing
  IBContact ctId -> Just ctId
  IBUser -> Just userCtId

data GroupMemberRole = GRMember | GRAdmin | GROwner
  deriving (Eq, Show, Ord)

instance FromField GroupMemberRole where fromField = fromBlobField_ toMemberRole

instance ToField GroupMemberRole where toField = toField . serializeMemberRole

toMemberRole :: ByteString -> Either String GroupMemberRole
toMemberRole = \case
  "owner" -> Right GROwner
  "admin" -> Right GRAdmin
  "member" -> Right GRMember
  r -> Left $ "invalid group member role " <> B.unpack r

serializeMemberRole :: GroupMemberRole -> ByteString
serializeMemberRole = \case
  GROwner -> "owner"
  GRAdmin -> "admin"
  GRMember -> "member"

fromBlobField_ :: Typeable k => (ByteString -> Either String k) -> FieldParser k
fromBlobField_ p = \case
  f@(Field (SQLBlob b) _) ->
    case p b of
      Right k -> Ok k
      Left e -> returnError ConversionFailed f ("couldn't parse field: " ++ e)
  f -> returnError ConversionFailed f "expecting SQLBlob column type"

data GroupMemberStatus
  = GSMemInvited -- member received (or sent to) invitation
  | GSMemAccepted -- member accepted invitation
  | GSMemConnected -- member created the group connection with the inviting member
  | GSMemReady -- member connections are forwarded to all previous members
  | GSMemFull -- member created group connections with all previous members
  deriving (Eq, Show, Ord)

instance FromField GroupMemberStatus where fromField = fromTextField_ memberStatusT

instance ToField GroupMemberStatus where toField = toField . serializeMemberStatus

memberStatusT :: Text -> Maybe GroupMemberStatus
memberStatusT = \case
  "invited" -> Just GSMemInvited
  "accepted" -> Just GSMemAccepted
  "connected" -> Just GSMemConnected
  "ready" -> Just GSMemReady
  "full" -> Just GSMemFull
  _ -> Nothing

serializeMemberStatus :: GroupMemberStatus -> Text
serializeMemberStatus = \case
  GSMemInvited -> "invited"
  GSMemAccepted -> "accepted"
  GSMemConnected -> "connected"
  GSMemReady -> "ready"
  GSMemFull -> "full"

data Connection = Connection
  { connId :: Int64,
    agentConnId :: ConnId,
    connLevel :: Int,
    viaContact :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    entityId :: Maybe Int64, -- contact or group member ID
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

data ConnStatus = ConnNew | ConnConfirmed | ConnAccepted | ConnReady
  deriving (Eq, Show)

instance FromField ConnStatus where fromField = fromTextField_ connStatusT

instance ToField ConnStatus where toField = toField . serializeConnStatus

connStatusT :: Text -> Maybe ConnStatus
connStatusT = \case
  "new" -> Just ConnNew
  "confirmed" -> Just ConnConfirmed
  "accepted" -> Just ConnAccepted
  "ready" -> Just ConnReady
  _ -> Nothing

serializeConnStatus :: ConnStatus -> Text
serializeConnStatus = \case
  ConnNew -> "new"
  ConnConfirmed -> "confirmed"
  ConnAccepted -> "accepted"
  ConnReady -> "ready"

data ConnType = ConnContact | ConnMember
  deriving (Eq, Show)

instance FromField ConnType where fromField = fromTextField_ connTypeT

instance ToField ConnType where toField = toField . serializeConnType

connTypeT :: Text -> Maybe ConnType
connTypeT = \case
  "contact" -> Just ConnContact
  "member" -> Just ConnMember
  _ -> Nothing

serializeConnType :: ConnType -> Text
serializeConnType = \case
  ConnContact -> "contact"
  ConnMember -> "member"

data NewConnection = NewConnection
  { agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64
  }
