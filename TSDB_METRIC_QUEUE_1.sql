--------------------------------------------------------
--  DDL for Table TSDB_METRIC_QUEUE
--------------------------------------------------------

  CREATE TABLE "TSDB_METRIC_QUEUE" 
   (	"METRIC_ID" NUMBER, 
	"M" "METRIC" 
   ) 
 NESTED TABLE "M"."TAGS" STORE AS "TSDB_METRIC_TAGSTORE"
 RETURN AS VALUE;

   COMMENT ON COLUMN "TSDB_METRIC_QUEUE"."METRIC_ID" IS 'A queued metric id to sort by';
   COMMENT ON COLUMN "TSDB_METRIC_QUEUE"."M" IS 'A queued metric';
   COMMENT ON TABLE "TSDB_METRIC_QUEUE"  IS 'A table that serves as a queue when HTTP POST/TELNET options are not not available or not used';
