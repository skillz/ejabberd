-- Run once on the fork DB before the first 26.09 start. Additive only; the 19.02 fork ignores these tables.
-- Without them update_sql_schema cannot place mod_muc_sql at a known version and ejabberd aborts at start.
-- Tables and indexes as in upstream sql/mysql.sql (tag 26.09); keys inline so a re-run is a no-op.
CREATE TABLE IF NOT EXISTS muc_online_room (
    name text NOT NULL,
    host text NOT NULL,
    node text NOT NULL,
    pid text NOT NULL,
    UNIQUE KEY i_muc_online_room_name_host (name(75), host(75)) USING BTREE
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE TABLE IF NOT EXISTS muc_online_users (
    username text NOT NULL,
    server text NOT NULL,
    resource text NOT NULL,
    name text NOT NULL,
    host text NOT NULL,
    node text NOT NULL,
    UNIQUE KEY i_muc_online_users (username(75), server(75), resource(75), name(75), host(75)) USING BTREE
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
