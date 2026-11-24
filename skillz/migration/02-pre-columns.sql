-- The columns 26.09 update_sql_schema would add, added ahead of the cutover while the fork still runs.
-- 26.09 then finds them, records the schema version and runs no DDL at start. The defaults stay, so the
-- fork's INSERTs (which omit these columns) work under any sql_mode, before and after a rollback.
--
-- archive: the FULLTEXT index i_text forces ALGORITHM=COPY, which blocks writes for the whole copy
-- (staging 32 GB: about 20 min locally). On a live DB run this statement through an online
-- schema-change tool (pt-online-schema-change: archive has UNIQUE NOT NULL `id`), not as plain SQL.
ALTER TABLE archive ADD COLUMN origin_id text NOT NULL DEFAULT ('');
CREATE INDEX i_archive_username_origin_id USING BTREE ON archive(username(191), origin_id(191)) ALGORITHM=INPLACE LOCK=NONE;

ALTER TABLE rosterusers ADD COLUMN approved boolean NOT NULL DEFAULT false;
