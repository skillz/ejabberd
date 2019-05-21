/**
  * This is a SQL migration to remove friends requests that can't be completed from when we had the active
  * P0 bug: ES-2123, post upgrade.
  *
  **/

DELETE FROM
    rosterusers
WHERE
    ask != "N"
    AND subscription != "B"
    AND (created_at >= "2019-04-08 00:00:00");
