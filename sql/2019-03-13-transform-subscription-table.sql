/**
  * This is a SQL migration to move data from the subscription table to the muc_room_subscribers table.
  *
  **/

/* Copy data. */
INSERT INTO 
    muc_room_subscribers (room, host, jid, nick, nodes, created_at)
SELECT 
    SUBSTRING_INDEX(room, '@', 1),
    SUBSTRING_INDEX(room, '@', -1),
    jid,
    SUBSTRING_INDEX(jid, '@', 1),
    '[<<"urn:xmpp:mucsub:nodes:messages">>,
  <<"urn:xmpp:mucsub:nodes:affiliations">>,<<"urn:xmpp:mucsub:nodes:subject">>,
  <<"urn:xmpp:mucsub:nodes:config">>]',
    created_at
FROM 
    subscription;
