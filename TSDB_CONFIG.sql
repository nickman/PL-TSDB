--------------------------------------------------------
--  DDL for Table TSDB_CONFIG
--------------------------------------------------------

  CREATE TABLE "TSDB_CONFIG" 
   (	"K" VARCHAR2(30), 
	"V" VARCHAR2(2000)
   ) SEGMENT CREATION DEFERRED 
  PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  TABLESPACE "USERS" ;

   COMMENT ON COLUMN "TSDB_CONFIG"."K" IS 'The PL/TSDB configuration item key';
   COMMENT ON COLUMN "TSDB_CONFIG"."V" IS 'The PL/TSDB configuration item value';
   COMMENT ON TABLE "TSDB_CONFIG"  IS 'The PL/TSDB configuration item key';
