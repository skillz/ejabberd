/**
  * This is a SQL migration to upgrade from version 17.07 -> 19.02, sequentially from the upgrade guide
  * found here: https://docs.ejabberd.im/admin/upgrade/#specific-version-upgrade-notes
  *
  **/

/**
  * 17.09 -> 17.11
  **/
DROP INDEX i_username ON archive;
CREATE INDEX i_username_timestamp USING BTREE ON archive(username,timestamp);

ALTER TABLE pubsub_node CHANGE type plugin text NOT NULL;

CREATE TABLE muc_room_subscribers (
   room varchar(191) NOT NULL,
   host varchar(191) NOT NULL,
   jid varchar(191) NOT NULL,
   nick text NOT NULL,
   nodes text NOT NULL,
   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY i_muc_room_subscribers_host_room_jid (host, room, jid)
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_muc_room_subscribers_host_jid USING BTREE ON muc_room_subscribers(host, jid);

CREATE TABLE push_session (
    username text NOT NULL,
    timestamp bigint NOT NULL,
    service text NOT NULL,
    node text NOT NULL,
    xml text NOT NULL
);

CREATE UNIQUE INDEX i_push_usn ON push_session (username(191), service(191), node(191));
CREATE UNIQUE INDEX i_push_ut ON push_session (username(191), timestamp);

/**
  * 18.01 -> 18.03
  **/
DROP INDEX i_username_timestamp ON archive;
DROP INDEX i_peer ON archive;
DROP INDEX i_bare_peer ON archive;
CREATE INDEX i_username_timestamp USING BTREE ON archive(username(191), timestamp);
CREATE INDEX i_username_peer USING BTREE ON archive(username(191), peer(191));
CREATE INDEX i_username_bare_peer USING BTREE ON archive(username(191), bare_peer(191));

/**
  * 18.03 -> 18.04
  **/
ALTER TABLE pubsub_item
 MODIFY creation varchar(32) NOT NULL,
 MODIFY modification varchar(32) NOT NULL;

/**
  * 18.04 -> 18.06
  */
DROP TABLE irc_custom;

/**
  * 18.06 -> 18.09
  */
ALTER TABLE spool MODIFY xml mediumtext NOT NULL;
ALTER TABLE archive MODIFY xml mediumtext NOT NULL;
ALTER TABLE archive MODIFY txt mediumtext;
ALTER TABLE pubsub_item MODIFY payload mediumtext NOT NULL;

/**
  * 18.09 -> 18.12
  **/
ALTER TABLE pubsub_item MODIFY payload mediumtext NOT NULL;
