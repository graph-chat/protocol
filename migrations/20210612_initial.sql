CREATE TABLE contact_profiles ( -- remote user profile
  contact_profile_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL, -- contact name set by remote user (not unique), this name must not contain spaces
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with contact profile properties
);

CREATE TABLE users (
  user_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL UNIQUE REFERENCES contacts ON DELETE CASCADE
    DEFERRABLE INITIALLY DEFERRED,
  local_display_name TEXT NOT NULL UNIQUE,
  active_user INTEGER NOT NULL DEFAULT 0, -- 1 for active user
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE display_names (
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL,
  ldn_base TEXT NOT NULL,
  ldn_suffix INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (user_id, local_display_name) ON CONFLICT FAIL,
  UNIQUE (user_id, ldn_base, ldn_suffix) ON CONFLICT FAIL
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER PRIMARY KEY,
  contact_profile_id INTEGER REFERENCES contact_profiles, -- NULL if it's an incognito profile
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL,
  is_user INTEGER NOT NULL DEFAULT 0, -- 1 if this contact is a user
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT,
  UNIQUE (user_id, local_display_name),
  UNIQUE (user_id, contact_profile_id)
);

CREATE TABLE known_servers(
  server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  user_id INTEGER NOT NULL REFERENCES users,
  UNIQUE (user_id, host, port)
) WITHOUT ROWID;

CREATE TABLE group_profiles ( -- shared group profiles
  group_profile_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL, -- this name must not contain spaces
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with user or contact profile
);

CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY, -- local group ID
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL, -- local group name without spaces
  group_profile_id INTEGER REFERENCES group_profiles, -- shared group profile
  inv_queue_info BLOB,
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT,
  UNIQUE (user_id, local_display_name),
  UNIQUE (user_id, group_profile_id)
);

CREATE TABLE group_members ( -- group members, excluding the local user
  group_member_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  member_id BLOB NOT NULL, -- shared member ID, unique per group
  member_role TEXT NOT NULL, -- owner, admin, member
  member_status TEXT NOT NULL, -- new, invited, accepted, connected, ready
  invited_by INTEGER REFERENCES contacts (contact_id) ON DELETE RESTRICT, -- NULL for the members who joined before the current user and for the group creator
  contact_profile_id INTEGER NOT NULL REFERENCES contact_profiles ON DELETE RESTRICT,
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  UNIQUE (group_id, member_id),
  UNIQUE (group_id, contact_id)
);

CREATE TABLE group_member_intros (
  group_member_intro_id INTEGER PRIMARY KEY,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  to_group_member_id INTEGER NOT NULL REFERENCES group_members (group_member_id) ON DELETE CASCADE,
  intro_status TEXT NOT NULL DEFAULT '', -- new, intro, inv, fwd, con
  UNIQUE (group_member_id, to_group_member_id)
);

CREATE TABLE connections ( -- all SMP agent connections
  connection_id INTEGER PRIMARY KEY,
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_contact INTEGER REFERENCES contacts (contact_id),
  conn_status TEXT NOT NULL,
  conn_type TEXT NOT NULL, -- contact, member, member_direct
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users
);

CREATE TABLE events ( -- messages received by the agent, append only
  event_id INTEGER PRIMARY KEY,
  agent_msg_id INTEGER NOT NULL, -- internal message ID
  external_msg_id INTEGER NOT NULL, -- external message ID (sent or received)
  agent_meta TEXT NOT NULL, -- JSON with timestamps etc. sent in MSG
  connection_id INTEGER NOT NULL REFERENCES connections,
  received INTEGER NOT NULL, -- 0 for received, 1 for sent
  chat_event_id INTEGER,
  continuation_of INTEGER, -- references chat_event_id, but can be incorrect
  event_type TEXT NOT NULL, -- event type - see protocol/types.ts
  event_encoding INTEGER NOT NULL, -- format of event_body: 0 - binary, 1 - text utf8, 2 - JSON (utf8)
  content_type TEXT NOT NULL, -- content type - see protocol/types.ts
  event_body BLOB, -- agent message body as sent
  event_hash BLOB NOT NULL,
  integrity TEXT NOT NULL DEFAULT '',
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX events_external_msg_id_index ON events (connection_id, external_msg_id);

CREATE TABLE event_body_parts (
  event_body_part_id INTEGER PRIMARY KEY,
  event_id REFERENCES events,
  full_size INTEGER NOT NULL,
  part_status TEXT, -- full, partial
  content_type TEXT NOT NULL,
  event_part BLOB
);

CREATE TABLE contact_profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  contact_profile_id INTEGER NOT NULL REFERENCES contact_profiles
);

CREATE TABLE group_profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  group_profile_id INTEGER NOT NULL REFERENCES group_profiles
);

CREATE TABLE group_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  group_member_id INTEGER REFERENCES group_members -- NULL for current user
);

CREATE TABLE group_event_parents (
  group_event_parent_id INTEGER PRIMARY KEY,
  event_id INTEGER NOT NULL REFERENCES group_events (event_id),
  parent_group_member_id INTEGER REFERENCES group_members (group_member_id), -- can be NULL if parent_member_id is incorrect
  parent_member_id BLOB, -- shared member ID, unique per group
  parent_event_id INTEGER REFERENCES events (event_id) ON DELETE CASCADE, -- this can be NULL if received event references another event that's not received yet
  parent_chat_event_id INTEGER NOT NULL,
  parent_event_hash BLOB NOT NULL
);

CREATE INDEX group_event_parents_parent_chat_event_id_index
  ON group_event_parents (parent_member_id, parent_chat_event_id);

CREATE TABLE messages ( -- mutable messages presented to user
  message_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT, -- 1 for sent messages
  group_id INTEGER REFERENCES groups ON DELETE RESTRICT, -- NULL for direct messages
  deleted INTEGER NOT NULL, -- 1 for deleted
  msg_type TEXT NOT NULL,
  content_type TEXT NOT NULL,
  msg_text TEXT NOT NULL, -- textual representation
  msg_props TEXT NOT NULL -- JSON
);

CREATE TABLE message_content (
  message_content_id INTEGER PRIMARY KEY,
  message_id INTEGER REFERENCES messages ON DELETE CASCADE,
  content_type TEXT NOT NULL,
  content_size INTEGER, -- full expected content size
  content_status TEXT, -- empty, part, full
  content BLOB NOT NULL
);

CREATE TABLE message_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  message_id INTEGER NOT NULL REFERENCES messages
);
