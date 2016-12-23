--------------------------------------------------------
--  Constraints for Table TSDB_METRIC_QUEUE
--------------------------------------------------------

  ALTER TABLE "TSDB_METRIC_QUEUE" MODIFY ("METRIC_ID" NOT NULL ENABLE);
  ALTER TABLE "TSDB_METRIC_QUEUE" ADD PRIMARY KEY ("METRIC_ID") ENABLE;
  ALTER TABLE "TSDB_METRIC_QUEUE" ADD UNIQUE ("M"."TAGS") ENABLE;
