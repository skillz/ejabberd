# ejabberd 19.02 fork -> 26.09 cutover runbook (DRAFT)

Status: draft from a local test on a staging MySQL dump (2026-11-24). Every step marked **[unverified]** was not run against a real environment. Items marked **[tested locally]** ran on the restored staging dump with the images from this branch.

Shape: blue/green. A new 26.09 StatefulSet on the same MySQL; the old 19.02 StatefulSet is scaled to 0 but kept. Never run both against the same DB at once.

## Files

| File | What | When |
|---|---|---|
| `01-pre-schema.sql` | creates `muc_online_room`, `muc_online_users` | before the first 26.09 start. Without them 26.09 aborts at start (`Can't update SQL schema for module mod_muc_sql`) [tested locally] |
| `02-pre-columns.sql` | adds `archive.origin_id` + index, `rosterusers.approved`, **with defaults** | days before the cutover, fork still running [tested locally: fork reads/writes on it; 26.09 then runs no DDL] |
| `rollback-data.sql` | makes 26.09-written data loadable by the fork | only on rollback, both clusters stopped [tested locally] |
| `run-migration-test.sh`, `verify.js` | the local test: restore a dump, run either server, read checks | before each environment |

## Config that must change (not only the chart translation in `skillz/conf/ejabberd.yml`)

- `mod_muc: preload_rooms: false`. The 26.09 default (`true`) reads every `muc_room` row and starts every room at boot: 1.9M rooms / 1.6 GB of opts on staging (local MySQL was OOM-killed). [tested locally]
- `update_sql_schema: true` is fine once 01 and 02 are applied: it only records versions. [tested locally]
- Keep the default SQL schema (no `new_sql_schema`): `archive` columns used by chat-insights-engine stay the same. [tested locally: no `server_host` added]

## Phase A: before the cutover day (fork keeps running)

1. Record the DB baseline: table sizes and row counts (`information_schema`), `select @@sql_mode` **(prod value unknown; it decides whether a rollback without defaults loses MAM writes)**. [unverified]
2. Apply `01-pre-schema.sql` (2 empty tables, instant). [tested locally]
3. Apply `02-pre-columns.sql`:
   - `archive` ADD COLUMN is `ALGORITHM=COPY` only (FULLTEXT `i_text` + expression default; INSTANT and INPLACE refused) and blocks writes for the whole copy: 41-86 s per 1.1 GB locally; staging full `archive` about 20-42 min; prod about 45-100 min if `archive` is about 74 GB (estimate from `aws.rds.volume_bytes_used`, **[unverified]**). Run it with `pt-online-schema-change` (archive has a UNIQUE NOT NULL `id`) or in a maintenance window. [tested locally: plain SQL; online tool unverified]
   - the index runs with `LOCK=NONE`. [tested locally]
   - Check the free storage first: COPY needs room for a second copy of `archive`. [unverified]
4. Soak: run the fork on that schema for a few days; watch MAM insert errors. [unverified]
5. Stage the 26.09 StatefulSet (replicas 0) with the new config; image built from this branch. [unverified]

## Phase B: cutover window (low traffic; confirm the hour in Datadog)

1. Suspend the `delete-unused-rooms` CronJob. [unverified]
2. Freeze admin-API writers if possible (quarantine changes in user-access-service / fraud-service retry or queue; confirm). [unverified]
3. Scale the 19.02 StatefulSet to 0. Wait until no pod is left (never both clusters on the DB). [unverified]
4. Start the 26.09 StatefulSet. Expect in the log: `schema_version` rows for mod_mam_sql 2, mod_roster_sql 2, mod_muc_sql 1, mod_offline_sql 1, mod_vcard_sql 1, mod_privacy_sql 1 and **no** `Add column` / `Create index` lines. One `Can't update SQL schema for module ejabberd_oauth_sql` error is expected (OAuth unused). Start time locally: 4 s. [tested locally]
5. Switch the external (5280) and admin (5290) Service selectors to the new pods. [unverified]
6. Clients reconnect by themselves (reconnect storm: every session re-authenticates through api-gateway `/chat-auth/`). [unverified]
7. Resume the CronJob only after the room-destroy policy is decided (26.09 deletes a room's archive on destroy unless configured; see the proof doc). [unverified]

## What to watch (first hour, then daily for 2 weeks)

- auth success/failure rate and api-gateway `/chat-auth/` latency;
- joins/min, groupchat messages/min, MAM inserts (rows/min in `archive`), push events/min;
- `mod_muc_room` crash reports and `[error]` count in the 26.09 log;
- admin API 4xx/5xx (fraud-service, user-details-service, chat-broadcast-service);
- MySQL CPU, connections, slow queries (P12 subscriptions query joins `archive`; P11 replicas are not ported);
- memory per pod (rooms load on demand and hibernate).

## Rollback trigger

Roll back if, after 30 min, any of: auth failure rate or join rate is off the baseline by more than 5%, message rate drops, push events stop, admin API calls fail, or `mod_muc_room` crashes repeat. After the 2-week watch, roll forward only. [thresholds unverified]

## Rollback steps

Do not just flip the selectors back: data written by 26.09 breaks the fork. [tested locally]

1. Scale 26.09 to 0; wait until no pod is left.
2. Run `rollback-data.sql` (restrict its `archive` DELETE to `timestamp >= <cutover time in microseconds>` on prod). Both check queries must return 0. Locally: 87 s on 1.9M rooms. It fixes:
   - room opts: 26.09 rewrites a room's opts when it starts it; the fork cannot load `subject_author = {Nick, #jid{}}` or `subject = [#text{}]` (crash in `mod_muc:opts_to_binary`). Without the fix, every room used on 26.09 fails to load on the fork;
   - `archive` rows for subject changes (no `<body>`): the fork's history-from-MAM at room start crashes on them;
   - column defaults (if 02 was not applied, or a 26.09 run dropped them): without them the fork's MAM inserts fail under a strict `sql_mode`.
3. Scale 19.02 up; switch the selectors back.
4. Verify: a game room join with history, a club room, a MUC/Sub list, a mute.

Limits of the rollback: room options that only 26.09 knows (hats, `allowpm`) are dropped by the fork; turn on no new 26.09 room features (XEP-0425 moderation, hats, XEP-0424) during the watch window. [tested locally for opts, subjects, messages, subscriptions; new features not tested]

## Not verified

Online schema tool on Aurora; real Aurora DDL timing; prod `sql_mode`, row counts and `archive` size; the chart/StatefulSet changes; selector switch and reconnect storm; push (ModPushSkillz) and pottymouth on 26.09; clustering; old native builds on devices.
