-- This file creates a new table, subscriptions, in the ejabberd DB.

USE ejabberd;

CREATE TABLE subscription (
    name varchar(50) NOT NULL,
    subscription text NOT NULL,
    last_updated timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_subscription_name_index USING BTREE on subscription(name(25));
CREATE INDEX i_subscription_subscription_index USING BTREE on subscription(subscription(25));
CREATE INDEX i_subscription_last_updated_index on subscription(last_updated);
