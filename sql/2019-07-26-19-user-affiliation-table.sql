/**
  * This is an SQL migration to allow storage of affiliations globally rather than per-muc-room.
  * Data will be back-filled from chat_user_quarantine after the code is changed to read/write from this table.
  **/
CREATE TABLE user_affiliation (
   id            BIGINT        NOT NULL AUTO_INCREMENT,
   version       BIGINT        NOT NULL,
   enabled       BIT DEFAULT 1 NOT NULL,
   user_id       BIGINT        NOT NULL,
   affiliation   VARCHAR(255)  NOT NULL,
   date_created  DATETIME      NOT NULL DEFAULT CURRENT_TIMESTAMP,
   last_updated  DATETIME      NOT NULL,
   PRIMARY KEY(id),
   INDEX user_affiliation_id_enabled_idx (user_id, enabled),
   INDEX user_affiliation_date_created_idx (date_created)
)
ENGINE = InnoDB
DEFAULT CHARSET = utf8;
