-- This file creates a new table, subscriptions, in the ejabberd DB.

USE ejabberd;

CREATE TABLE subscriptions (
    name text NOT NULL,
    subscription text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE INDEX i_subscriptions_name_index USING BTREE on subscriptions(name(25));
CREATE INDEX i_subscriptions_subscription_index USING BTREE on subscriptions(subscription(25));
CREATE INDEX i_subscriptions_created_at_index on subscriptions(created_at);
