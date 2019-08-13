/**
  * This is an SQL migration to allow storage of affiliations globally rather than per-muc-room.
  * Data will be back-filled from chat_user_quarantine after the code is changed to read/write from this table.
  **/
CREATE TABLE user_affiliation (
   id            BIGINT        NOT NULL AUTO_INCREMENT,
   version       BIGINT        NOT NULL,
   enabled       BIT DEFAULT 1 NOT NULL,
   nick          VARCHAR(127)  NOT NULL,
   affiliation   VARCHAR(30)   NOT NULL COLLATE utf8mb4_unicode_ci,
   date_created  DATETIME      NOT NULL DEFAULT CURRENT_TIMESTAMP,
   last_updated  DATETIME      NOT NULL DEFAULT CURRENT_TIMESTAMP,
   PRIMARY KEY(id),
   INDEX user_affiliation_enabled_nick_idx (enabled, nick),
   INDEX user_affiliation_date_created_idx (date_created)
)
ENGINE = InnoDB
DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
