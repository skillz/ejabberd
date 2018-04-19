-- This file creates a new table, subscription, in the ejabberd DB.

USE ejabberd;

CREATE TABLE subscription (
    jid varchar(50) NOT NULL,
    room varchar(255) NOT NULL,
    last_updated timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_subscription_name_index USING BTREE on subscription(name(25));
CREATE INDEX i_subscription_created_at_index on subscription(created_at);
