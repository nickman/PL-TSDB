--------------------------------------------------------
--  Constraints for Table TSDB_CONFIG
--------------------------------------------------------

  ALTER TABLE "TSDB_CONFIG" MODIFY ("K" NOT NULL ENABLE);
  ALTER TABLE "TSDB_CONFIG" MODIFY ("V" NOT NULL ENABLE);
  ALTER TABLE "TSDB_CONFIG" ADD PRIMARY KEY ("K") ENABLE;
