--------------------------------------------------------
--  DDL for Sequence TSDB_METRIC_SEQ
--------------------------------------------------------

CREATE SEQUENCE  TSDB_METRIC_SEQ  MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY 1 START WITH 1 CACHE 200 NOORDER  NOCYCLE;

--------------------------------------------------------
--  DDL for Table TSDB_METRIC_QUEUE
--------------------------------------------------------

CREATE TABLE TSDB_METRIC_QUEUE (
  METRIC_ID NUMBER PRIMARY KEY NOT NULL, 
	M METRIC
)
NESTED TABLE M.TAGS STORE AS TSDB_METRIC_TAGSTORE
  RETURN AS VALUE;
COMMENT ON COLUMN TSDB_METRIC_QUEUE.METRIC_ID IS 'A queued metric id to sort by';
COMMENT ON COLUMN TSDB_METRIC_QUEUE.M IS 'A queued metric';
COMMENT ON TABLE TSDB_METRIC_QUEUE  IS 'A table that serves as a queue when HTTP POST/TELNET options are not not available or not used';

--------------------------------------------------------
--  DDL for Type METRIC_EXT
--------------------------------------------------------

  CREATE OR REPLACE TYPE METRIC_EXT FORCE AS OBJECT ( 
  METRIC_ID NUMBER,
  XROWID VARCHAR2(18),
  METRIC_STRING VARCHAR2(1800),
  CONSTRUCTOR FUNCTION METRIC_EXT(id IN NUMBER, xrow IN VARCHAR2, met IN METRIC) RETURN SELF AS RESULT
);

/

CREATE OR REPLACE TYPE BODY METRIC_EXT AS

  CONSTRUCTOR FUNCTION METRIC_EXT(id IN NUMBER, xrow IN VARCHAR2, met IN METRIC) RETURN SELF AS RESULT AS
  BEGIN
    SELF.METRIC_ID := id;
    SELF.XROWID := xrow;
    SELF.METRIC_STRING := met.JSONMS();
    RETURN;
  END METRIC_EXT;
END;

/

--------------------------------------------------------
--  DDL for Type METRIC_EXT_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE METRIC_EXT_ARR as table of metric_ext

/






