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
    AND (created_at BETWEEN "2019-04-08 00:00:00" AND "2019-04-24 00:00:00");
