-- Rollback only: make data written by 26.09 loadable by the 19.02 fork again.
-- Room opts: rewrite terms the fork cannot load.
-- 26.09 stores subject_author as {Nick, #jid{}} and subject as [#text{}]; the fork's
-- mod_muc:opts_to_binary crashes on the tuple, so every room 26.09 started fails to load on 19.02.
-- Keys the fork does not know (enable_hats, hats_*, roles, salt, hibernation_time, allowpm) are left:
-- the fork skips unknown keys. allowpm is not mapped back; the fork default applies.
-- Run with both clusters stopped. Then the check query below must return 0.
UPDATE muc_room
SET opts = REGEXP_REPLACE(
      REGEXP_REPLACE(
        REGEXP_REPLACE(opts,
          '\\{subject_author,\\{(<<[^>]*>>),\\{jid(,<<[^>]*>>){6}\\}\\}\\}', '{subject_author,$1}'),
        '\\{subject,\\[\\{text,<<[^>]*>>,(<<[^>]*>>)\\}[^]]*\\]\\}', '{subject,$1}'),
      '\\{subject,\\[\\]\\}', '{subject,<<>>}')
WHERE opts LIKE '%{subject_author,{%' OR opts LIKE '%{subject,[%';

-- 26.09 archives subject changes (no <body>); the fork's room start (history from MAM) crashes on them.
-- shortcut: full scan of archive; on prod restrict by timestamp >= the cutover time.
DELETE FROM archive WHERE xml NOT LIKE '%<body%' AND xml LIKE '%<subject%';

-- 26.09 drops the defaults of its new NOT NULL columns; the fork's INSERTs omit them and fail
-- under a strict sql_mode. Both statements are metadata-only (INSTANT).
ALTER TABLE archive MODIFY origin_id text NOT NULL DEFAULT (''), ALGORITHM=INSTANT;
ALTER TABLE rosterusers ALTER COLUMN approved SET DEFAULT false, ALGORITHM=INSTANT;

-- check: rows the fork still cannot load (both must be 0)
SELECT COUNT(*) AS opts_not_converted FROM muc_room
WHERE opts LIKE '%{subject_author,{%' OR opts LIKE '%{subject,[%';
SELECT COUNT(*) AS bodyless_archive_rows FROM archive WHERE xml NOT LIKE '%<body%';
