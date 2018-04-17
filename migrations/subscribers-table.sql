-- This file creates a new table, subscriptions, in the ejabberd DB.

CREATE TABLE subscriptions (
    name text NOT NULL,
    subscriber_list mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE UNIQUE INDEX i_subscriber_name_index USING BTREE on subscriptions(name(25));
CREATE UNIQUE INDEX i_subscriber_created_at_index on subscriptions(created_at);
