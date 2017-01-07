--------------------------------------------------------
--  DDL for Table TSDB_CONFIG
--------------------------------------------------------

  CREATE TABLE TSDB_CONFIG (
    K VARCHAR2(30) PRIMARY KEY NOT NULL, 
    V VARCHAR2(2000) NOT NULL
  );
  COMMENT ON COLUMN TSDB_CONFIG.K IS 'The PL/TSDB configuration item key';
  COMMENT ON COLUMN TSDB_CONFIG.V IS 'The PL/TSDB configuration item value';
  COMMENT ON TABLE TSDB_CONFIG  IS 'The PL/TSDB configuration item key';

--------------------------------------------------------
--  DDL for Type VARCHAR2_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE VARCHAR2_ARR FORCE IS TABLE OF VARCHAR2(200);

/

--------------------------------------------------------
--  DDL for Type INT_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE INT_ARR FORCE IS TABLE OF INT;

/

--------------------------------------------------------
--  DDL for Type NAMEVALUE
--------------------------------------------------------

create or replace TYPE NAMEVALUE AS OBJECT (
  NAME VARCHAR2(360),
  VALUE NUMBER
);

/

--------------------------------------------------------
--  DDL for Type NAMEVALUE_ARR
--------------------------------------------------------

create or replace type NAMEVALUE_ARR AS TABLE OF NAMEVALUE;

/  

--------------------------------------------------------
--  DDL for Package TSDB_UTIL
--------------------------------------------------------

create or replace PACKAGE TSDB_UTIL AS 

  -- Generic Ref Cursor
  TYPE TRACECUR IS REF CURSOR;
  -- Timestamp instance to extract stuff from but is unchanging
  BASETS CONSTANT TIMESTAMP WITH TIME ZONE := SYSTIMESTAMP;
  -- The timzone offset hours
  TZHOUR CONSTANT PLS_INTEGER := extract(TIMEZONE_HOUR from BASETS);
  -- The timzone offset hours
  TZMINUTE CONSTANT PLS_INTEGER := extract(TIMEZONE_MINUTE from BASETS);  
  -- The epoch timestamp
  EPOCH CONSTANT TIMESTAMP := to_timestamp_tz('1970-01-01 ' || TZHOUR || ':' || TZMINUTE, 'YYYY-MM-DD TZH:TZM');  
  -- TZ Offset hours/minutes converted to seconds
  TZOFFSETSECS CONSTANT PLS_INTEGER := (TZHOUR*60*60) + (TZMINUTE*60);
  -- TZ Offset hours/minutes converted to ms
  TZOFFSETMS CONSTANT PLS_INTEGER := TZOFFSETSECS * 1000;
  -- The default max buffer size for TSDBM instances
  MAX_BUFFER_SIZE CONSTANT INT := 32767;
  -- EOL Char
  EOL VARCHAR2(2) := '
';

  --===================================================================================================================
  --  Logging level constants: -- Valid logging levels: 0: OFF, 1: ERROR, 2: INFO, 3:ALL
  --===================================================================================================================
  LEVEL_OFF CONSTANT PLS_INTEGER := 0;
  LEVEL_ERROR CONSTANT PLS_INTEGER := 1;
  LEVEL_INFO CONSTANT PLS_INTEGER := 2;
  LEVEL_ALL CONSTANT PLS_INTEGER := 3;

  --===================================================================================================================
  --  Logging type constants
  --===================================================================================================================
  LOG_TABLE CONSTANT VARCHAR2(5) := 'TABLE';
  LOG_ALERT CONSTANT VARCHAR2(5) := 'ALERT';
  LOG_TCP CONSTANT VARCHAR2(3) := 'TCP';
  LOG_HTTP CONSTANT VARCHAR2(4) := 'HTTP';

  --===================================================================================================================
  --  Tracing type constants
  --===================================================================================================================
  TRACE_HTTP CONSTANT VARCHAR2(4) := 'HTTP';
  TRACE_TELNET CONSTANT VARCHAR2(6) := 'TELNET';
  TRACE_TABLE CONSTANT VARCHAR2(5) := 'TABLE';
  
  --============================================================================
  -- Converts a number to varchar2, prepending a 0 if requiured
  --============================================================================
  FUNCTION NTOV(val IN NUMBER) RETURN VARCHAR2;


  --============================================================================
  -- Returns the configured logging level
  --============================================================================
  FUNCTION CFG_LOGGING_LEVEL RETURN PLS_INTEGER;

  --============================================================================
  -- Returns the configured logging type
  --============================================================================
  FUNCTION CFG_LOGGING_TYPE RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured tracing type
  --============================================================================
  FUNCTION CFG_TRACING_TYPE RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured http tracing endpoint host
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_HOST RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured http tracing endpoint uri
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_URI RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured http tracing endpoint port
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_PORT RETURN INT;
  
  --============================================================================
  -- Returns the configured telnet tracing endpoint host
  --============================================================================
  FUNCTION CFG_TRACING_TELNET_HOST RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured http telnet endpoint port
  --============================================================================
  FUNCTION CFG_TRACING_TELNET_PORT RETURN INT;
  
  --============================================================================
  -- Returns the configured HTTP logging endpoint host
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_HOST RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured HTTP logging endpoint port
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_PORT RETURN INT;
  
  --============================================================================
  -- Returns the configured HTTP logging URI
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_URI RETURN VARCHAR2;
  
  
  --===================================================================================================================
  --  Indicates if the passed logging level is enabled: 0: OFF, 1: ERROR, 2: INFO, 3:ALL
  --===================================================================================================================
  FUNCTION IS_LOG_ENABLED(level IN PLS_INTEGER) RETURN BOOLEAN;
  
  
  --============================================================================
  -- Returns the configured telnet logging endpoint host
  --============================================================================
  FUNCTION CFG_LOGGING_TELNET_HOST RETURN VARCHAR2;

  --============================================================================
  -- Returns the configured logging endpoint port
  --============================================================================
  FUNCTION CFG_LOGGING_TELNET_PORT RETURN INT;
  
  --============================================================================
  -- Returns the configured telnet logging endpoint host
  --============================================================================
  FUNCTION CFG_LOGGING_TS_FORMAT RETURN VARCHAR2;
  
  --============================================================================
  -- Returns the configured DBMS_ALERT alert name
  --============================================================================
  FUNCTION CFG_LOGGING_ALERT_NAME RETURN VARCHAR2;  


  --===================================================================================
  -- Clears all stored configuration and replaces it with the defaults
  --===================================================================================
  PROCEDURE INSTALL_CFG_DEFAULTS;
  
  --===================================================================================================================
  --  [Re]Loads the configuration
  --===================================================================================================================
  PROCEDURE LOAD_CONFIG;  
  
  --============================================================================
  -- Sets the configured logging type
  --============================================================================  
  PROCEDURE SET_CFG_LOGGING_TYPE(ltype IN VARCHAR2, persist IN INT DEFAULT 0);  
  
  --===================================================================================================================
  --  Returns the cleaned user statistic keys
  --===================================================================================================================
  FUNCTION USERSTATKEYS RETURN VARCHAR2_ARR;
  

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN NUMBER;

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN NUMBER;
  
  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN TIMESTAMP WITH TIME ZONE DEFAULT SYSTIMESTAMP) RETURN NUMBER;

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN TIMESTAMP WITH TIME ZONE DEFAULT SYSTIMESTAMP) RETURN NUMBER;
  
  
  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSDATE if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN DATE DEFAULT SYSDATE) RETURN NUMBER;

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSDATE if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN DATE DEFAULT SYSDATE) RETURN NUMBER;
  
  --===================================================================================================================
  --  Determines how many digits are in the passed number
  --===================================================================================================================
  FUNCTION DIGITS(n IN NUMBER DEFAULT 0) RETURN INT;  
  
  --===================================================================================================================
  --  Determines if the passed number is most likely SECONDS or MILLISECONDS and returns the value in MILLISECONDS
  --===================================================================================================================
  FUNCTION TOMS(n IN NUMBER DEFAULT 0) RETURN NUMBER;  
  
  --===================================================================================================================
  --  Determines if the passed number is most likely SECONDS or MILLISECONDS and returns the value in SECONDS
  --===================================================================================================================
  FUNCTION TOSEC(n IN NUMBER DEFAULT 0) RETURN NUMBER;  
  
  --==================================================================================================
  -- Converts the passed date to a timestamp with as high precision as possible.
  -- The passed date defaults to 'SYSDATE'
  -- Additional fractional seconds can be specified to add precision.
  --==================================================================================================
  FUNCTION DATETOTIMESTAMP(dt IN DATE DEFAULT SYSDATE, fs IN INTEGER DEFAULT 0) RETURN TIMESTAMP;
  
  --===================================================================================================================
  --  Returns the current time as the number of milliseconds since epoch (like java.lang.System.getCurrentTimeMillis())
  --===================================================================================================================
  FUNCTION CURRENTMS RETURN NUMBER;
  
  --===================================================================================================================
  --  Returns the current time as the number of seconds since epoch (unix time)
  --===================================================================================================================
  FUNCTION CURRENTSEC RETURN NUMBER;
  
  --===================================================================================================================
  --  Returns the sid for the current session
  --===================================================================================================================
  FUNCTION SESSION_SID RETURN NUMBER;
  
  --===================================================================================================================
  --  Returns the database name
  --===================================================================================================================
  FUNCTION DB_NAME RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Returns the database host
  --===================================================================================================================
  FUNCTION DB_HOST RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Returns the database IP address
  --===================================================================================================================
  FUNCTION DB_IP RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Returns the host name of the connecting client this session was initiated by
  --===================================================================================================================
  FUNCTION CLIENT_HOST RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Returns the db user name for this session
  --===================================================================================================================
  FUNCTION DB_USER RETURN VARCHAR2;  
  
  --===================================================================================================================
  --  Cleans the passed string to remove whitespace, lowercase and illegal punctuation
  --===================================================================================================================
  FUNCTION CLEAN(str IN VARCHAR2, repl IN CHAR DEFAULT '.') RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Cleans the passed string to remove whitespace, lowercase and illegal punctuation
  --===================================================================================================================
  PROCEDURE CLEAN(str IN OUT NOCOPY VARCHAR2);
  
  
END TSDB_UTIL;

/

--------------------------------------------------------
--  DDL for Package Body TSDB_UTIL
--------------------------------------------------------

create or replace PACKAGE BODY TSDB_UTIL AS

  /* The SID of the current session */
  sid NUMBER;
  /* The DB Name */
  dbName VARCHAR2(30);
  /* The DB host name */
  dbHost VARCHAR2(30);  
  /* The DB user name */
  dbUser VARCHAR2(30);    
  /* The DB IP Address */
  dbIp VARCHAR2(30);  
  /* The host name of the connected client */
  clientHost VARCHAR2(30);
  /* The user level stats names */
  userStatKeys_ VARCHAR2_ARR := VARCHAR2_ARR(
  'buffer_is_not_pinned_count',
  'bytes_received_via_sqlnet_from_client',
  'bytes_sent_via_sqlnet_to_client',
  'calls_to_get_snapshot_scn:_kcmgss',
  'calls_to_kcmgcs',
  'ccursor_sql_area_evicted',
  'consistent_gets',
  'consistent_gets_examination',
  'consistent_gets_examination_fastpath',
  'consistent_gets_from_cache',
  'consistent_gets_pin',
  'consistent_gets_pin_fastpath',
  'cpu_used_by_this_session',
  'cpu_used_when_call_started',
  'cursor_authentications',
  'db_time',
  'enqueue_releases',
  'enqueue_requests',
  'execute_count',
  'index_fetch_by_key',
  'index_scans_kdiixs1',
  'logical_read_bytes_from_cache',
  'no_work_consistent_read_gets',
  'nonidle_wait_count',
  'opened_cursors_cumulative',
  'parse_count_hard',
  'parse_count_total',
  'parse_time_elapsed',
  'recursive_calls',
  'recursive_cpu_usage',
  'requests_to_from_client',
  'rows_fetched_via_callback',
  'session_cursor_cache_count',
  'session_cursor_cache_hits',
  'session_logical_reads',
  'sorts_memory',
  'sorts_rows',
  'sqlnet_roundtrips_to_from_client',
  'table_fetch_by_rowid',
  'user_calls',
  'workarea_executions_optimal');  
  

  --===================================================================================================================
  --  The tracing config entries
  --===================================================================================================================
  -- Trace Type Key
  CONFIG_TRACING_TYPE CONSTANT VARCHAR2(30) := 'tracing.type';  
  -- Default Trace Type
  DEFAULT_TRACING_TYPE CONSTANT VARCHAR2(30) := TRACE_HTTP;
  -- Valid tracing types
  VALID_TRACING_TYPES CONSTANT VARCHAR2_ARR := VARCHAR2_ARR(TRACE_HTTP, TRACE_TELNET, TRACE_TABLE);  
  --===================================================================================================================
  --  HTTP tracing config 
  --===================================================================================================================  
  -- HTTP/JSON Tracer Host Key
  CONFIG_TRACING_HTTP_HOST CONSTANT VARCHAR2(30) := 'tracing.http.host';
  -- Default HTTP/JSON Tracer Host
  DEFAULT_TRACING_HTTP_HOST CONSTANT VARCHAR2(30) := 'localhost';
  -- HTTP/JSON Tracer Port Key
  CONFIG_TRACING_HTTP_PORT CONSTANT VARCHAR2(30) := 'tracing.http.port';
  -- Default HTTP/JSON Tracer Port
  DEFAULT_TRACING_HTTP_PORT CONSTANT INT := 4242;
  -- HTTP/JSON Tracer URI
  CONFIG_TRACING_HTTP_URI CONSTANT VARCHAR2(30) := 'tracing.http.uri';
  -- Default HTTP/JSON Tracer URI
  DEFAULT_TRACING_HTTP_URI CONSTANT VARCHAR2(30) := '/api/put';
  --===================================================================================================================
  --  Telnet tracing config 
  --===================================================================================================================  
  -- Telnet Tracer Host Key
  CONFIG_TRACING_TELNET_HOST CONSTANT VARCHAR2(30) := 'tracing.telnet.host';
  -- Default Telnet Tracer Host
  DEFAULT_TRACING_TELNET_HOST CONSTANT VARCHAR2(30) := 'localhost';
  -- Telnet Tracer Port Key
  CONFIG_TRACING_TELNET_PORT CONSTANT VARCHAR2(30) := 'tracing.telnet.port';
  -- Default Telnet Tracer Port
  DEFAULT_TRACING_TELNET_PORT CONSTANT INT := 4242;


  
  --===================================================================================================================
  --  The logging config entries
  --===================================================================================================================
  -- Logging Type Key
  CONFIG_LOGGING_TYPE CONSTANT VARCHAR2(30) := 'logging.type';
  -- Logging Trace Type
  DEFAULT_LOGGING_TYPE CONSTANT VARCHAR2(5) := LOG_TABLE; 
  -- Valid logging types
  VALID_LOGGING_TYPES CONSTANT VARCHAR2_ARR := VARCHAR2_ARR(LOG_TABLE, LOG_ALERT, LOG_TCP, LOG_HTTP);
  -- Logging Level Key
  CONFIG_LOGGING_LEVEL CONSTANT VARCHAR2(30) := 'logging.level';
  -- Default Logging Level
  DEFAULT_LOGGING_LEVEL CONSTANT PLS_INTEGER := 2; 
  -- Valid logging levels: 0: OFF, 1: ERROR, 2: INFO, 3:ALL
  VALID_LOGGING_LEVELS CONSTANT INT_ARR := INT_ARR(LEVEL_OFF, LEVEL_ERROR, LEVEL_INFO, LEVEL_ALL);

  -- Logging Timestamp Format Key
  CONFIG_LOGGING_TSFORMAT CONSTANT VARCHAR2(30) := 'logging.tsformat';
  -- Default Logging Timestamp Format
  DEFAULT_LOGGING_TSFORMAT CONSTANT VARCHAR2(30) := 'YY/MM/DD HH24:MI:SS,FF3';
  
  --===================================================================================================================
  --  Telnet tracing config 
  --===================================================================================================================  
  -- Telnet Logging Host Key
  CONFIG_LOGGING_TELNET_HOST CONSTANT VARCHAR2(30) := 'logging.telnet.host';
  -- Default Telnet Logging Host
  DEFAULT_LOGGING_TELNET_HOST CONSTANT VARCHAR2(30) := 'localhost';
  -- Telnet Logging Port Key
  CONFIG_LOGGING_TELNET_PORT CONSTANT VARCHAR2(30) := 'logging.telnet.port';
  -- Default Telnet Logging Port
  DEFAULT_LOGGING_TELNET_PORT CONSTANT INT := 1234;
  
  -- DBMS_ALERT Logging Alert Name Key
  CONFIG_LOGGING_ALERT_NAME CONSTANT VARCHAR2(30) := 'logging.alert.name';
  -- Default DBMS_ALERT Logging Alert Name
  DEFAULT_LOGGING_ALERT_NAME CONSTANT VARCHAR2(30) := 'TSDB_LOG_ALERT';
  
  --===================================================================================================================
  --  HTTP Logging config 
  --===================================================================================================================  
  -- HTTP/JSON Logging Host Key
  CONFIG_LOGGING_HTTP_HOST CONSTANT VARCHAR2(30) := 'logging.http.host';
  -- Default HTTP/JSON Logging Host
  DEFAULT_LOGGING_HTTP_HOST CONSTANT VARCHAR2(30) := 'localhost';
  -- HTTP/JSON Logging Port Key
  CONFIG_LOGGING_HTTP_PORT CONSTANT VARCHAR2(30) := 'logging.http.port';
  -- Default HTTP/JSON Logging Port
  DEFAULT_LOGGING_HTTP_PORT CONSTANT INT := 2345;
  -- HTTP/JSON Logging URI
  CONFIG_LOGGING_HTTP_URI CONSTANT VARCHAR2(30) := 'logging.http.uri';
  -- Default HTTP/JSON Logging URI
  DEFAULT_LOGGING_HTTP_URI CONSTANT VARCHAR2(30) := '/tsdb/log';
  
  
  
  -- Configured Trace Type
  tracingType VARCHAR2(2000);
  -- Configured Trace HTTP/JSON Tracer Host
  httpTracerHost VARCHAR2(2000);
  -- Configured Trace HTTP/JSON Tracer Port
  httpTracerPort INT;
  -- Configured Trace HTTP/JSON Tracer URI
  httpTracerUri VARCHAR2(2000);
  -- Configured Trace Telnet Tracer Host
  telnetTracerHost VARCHAR2(2000);
  -- Configured Trace Telnet Tracer Port
  telnetTracerPort INT;
  
  -- Configured Logging Timestamp Format
  loggingTimestampFormat VARCHAR2(30);
  
  -- Configured Logging Type
  loggingType VARCHAR2(2000);
  -- Configured Logging Level
  loggingLevel PLS_INTEGER;
  
  -- Configured HTTP/JSON Logging Host
  httpLoggingHost VARCHAR2(2000);
  -- Configured HTTP/JSON Logging Port
  httpLoggingPort INT;
  -- Configured HTTP/JSON Logging URI
  httpLoggingUri VARCHAR2(2000);
  
  
  -- Configured Logging Telnet Host
  telnetLoggingHost VARCHAR2(2000);
  -- Configured Logging Telnet Port
  telnetLoggingPort INT;

  -- Configured Logging DBMS_ALERT Alert Name
  alertLoggingName VARCHAR2(2000);

  --============================================================================
  -- Converts a number to varchar2, prepending a 0 if requiured
  --============================================================================
  FUNCTION NTOV(val IN NUMBER) RETURN VARCHAR2 IS
  BEGIN
    IF(val IS NULL) THEN RETURN '0'; END IF;
    IF(val < 1 AND val > 0) THEN
      RETURN '0' || val;
    END IF;
    RETURN '' || val;
  END NTOV;


  --============================================================================
  -- Returns the configured logging level
  --============================================================================
  FUNCTION CFG_LOGGING_LEVEL RETURN PLS_INTEGER IS
  BEGIN
    RETURN loggingLevel;
  END CFG_LOGGING_LEVEL;

  --============================================================================
  -- Returns the configured logging type
  --============================================================================
  FUNCTION CFG_LOGGING_TYPE RETURN VARCHAR2 IS
  BEGIN
    RETURN loggingType;
  END CFG_LOGGING_TYPE;

  --============================================================================
  -- Sets the configured logging type
  --============================================================================  
  PROCEDURE SET_CFG_LOGGING_TYPE(ltype IN VARCHAR2, persist IN INT DEFAULT 0) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    typecheck INT;
    lt CONSTANT VARCHAR2(30) := UPPER(TRIM(NVL(ltype, 'X')));
  BEGIN
    IF(lt='X') THEN
      RAISE_APPLICATION_ERROR(-20101, 'Null Logging Type');
    END IF;
    IF(loggingType = lt) THEN
      RETURN;
    END IF;
    SELECT COUNT(*) INTO typecheck FROM TABLE(VALID_LOGGING_TYPES) WHERE COLUMN_VALUE = lt;
    IF(typecheck=0) THEN
      RAISE_APPLICATION_ERROR(-20101, 'Invalid Logging Type: [' || ltype || ']');
    END IF;
    loggingType := lt;
    IF(persist=1) THEN
      -- CONFIG_LOGGING_TYPE
      MERGE INTO TSDB_CONFIG T
      USING (SELECT CONFIG_LOGGING_TYPE INTYPE FROM DUAL) INCOMING
      ON (T.K = INCOMING.INTYPE)
      WHEN MATCHED THEN
        UPDATE SET V = lt 
      WHEN NOT MATCHED THEN
      INSERT (T.K, T.V)
      VALUES (CONFIG_LOGGING_TYPE, lt);
    END IF;    
    COMMIT;
  END SET_CFG_LOGGING_TYPE;
  
  
  --============================================================================
  -- Returns the configured tracing type
  --============================================================================
  FUNCTION CFG_TRACING_TYPE RETURN VARCHAR2 IS
  BEGIN
    RETURN tracingType;
  END CFG_TRACING_TYPE;

  --============================================================================
  -- Returns the configured http tracing endpoint host
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN httpTracerHost;
  END CFG_TRACING_HTTP_HOST;

  --============================================================================
  -- Returns the configured http tracing endpoint uri
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_URI RETURN VARCHAR2 IS
  BEGIN
    RETURN httpTracerUri;
  END CFG_TRACING_HTTP_URI;

  --============================================================================
  -- Returns the configured http tracing endpoint port
  --============================================================================
  FUNCTION CFG_TRACING_HTTP_PORT RETURN INT IS
  BEGIN
    RETURN httpTracerPort;
  END CFG_TRACING_HTTP_PORT;
  
  
  --============================================================================
  -- Returns the configured telnet tracing endpoint host
  --============================================================================
  FUNCTION CFG_TRACING_TELNET_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN telnetTracerHost;
  END CFG_TRACING_TELNET_HOST;

  --============================================================================
  -- Returns the configured telnet endpoint port
  --============================================================================
  FUNCTION CFG_TRACING_TELNET_PORT RETURN INT IS
  BEGIN
    RETURN telnetTracerPort;
  END CFG_TRACING_TELNET_PORT;
  
  --============================================================================
  -- Returns the configured TCP (telnet) logging endpoint host
  --============================================================================
  FUNCTION CFG_LOGGING_TELNET_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN telnetLoggingHost;
  END CFG_LOGGING_TELNET_HOST;

  --============================================================================
  -- Returns the configured TCP (telnet) logging endpoint port
  --============================================================================
  FUNCTION CFG_LOGGING_TELNET_PORT RETURN INT IS
  BEGIN
    RETURN telnetLoggingPort;
  END CFG_LOGGING_TELNET_PORT;
  
  --============================================================================
  -- Returns the configured logging timestamp format
  --============================================================================
  FUNCTION CFG_LOGGING_TS_FORMAT RETURN VARCHAR2 IS
  BEGIN
    RETURN loggingTimestampFormat;
  END CFG_LOGGING_TS_FORMAT;
  
  --============================================================================
  -- Returns the configured DBMS_ALERT alert name
  --============================================================================
  FUNCTION CFG_LOGGING_ALERT_NAME RETURN VARCHAR2 IS
  BEGIN
    RETURN alertLoggingName;
  END CFG_LOGGING_ALERT_NAME;
  
  --============================================================================
  -- Returns the configured HTTP logging endpoint host
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN httpLoggingHost;
  END CFG_LOGGING_HTTP_HOST;

  --============================================================================
  -- Returns the configured HTTP logging endpoint port
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_PORT RETURN INT IS
  BEGIN
    RETURN httpLoggingPort;
  END CFG_LOGGING_HTTP_PORT;
  
  --============================================================================
  -- Returns the configured HTTP logging URI
  --============================================================================
  FUNCTION CFG_LOGGING_HTTP_URI RETURN VARCHAR2 IS
  BEGIN
    RETURN httpLoggingUri;
  END CFG_LOGGING_HTTP_URI;
  
  
  --===================================================================================
  -- Clears all stored configuration and replaces it with the defaults
  --===================================================================================
  PROCEDURE INSTALL_CFG_DEFAULTS IS 
    PRAGMA AUTONOMOUS_TRANSACTION;        
  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE TSDB_CONFIG';
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_TYPE, DEFAULT_TRACING_TYPE);

    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_HTTP_HOST, DEFAULT_TRACING_HTTP_HOST);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_HTTP_PORT, TO_CHAR(DEFAULT_TRACING_HTTP_PORT));
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_HTTP_URI, DEFAULT_TRACING_HTTP_URI);

    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_TELNET_HOST, DEFAULT_TRACING_TELNET_HOST);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_TRACING_TELNET_PORT, TO_CHAR(DEFAULT_TRACING_TELNET_PORT));
    
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_TYPE, DEFAULT_LOGGING_TYPE);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_LEVEL, DEFAULT_LOGGING_LEVEL);
    
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_TELNET_HOST, DEFAULT_LOGGING_TELNET_HOST);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_TELNET_PORT, TO_CHAR(DEFAULT_LOGGING_TELNET_PORT));
    
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_TSFORMAT, DEFAULT_LOGGING_TSFORMAT);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_ALERT_NAME, DEFAULT_LOGGING_ALERT_NAME);

    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_HTTP_HOST, DEFAULT_LOGGING_HTTP_HOST);
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_HTTP_PORT, TO_CHAR(DEFAULT_LOGGING_HTTP_PORT));
    INSERT INTO TSDB_CONFIG VALUES(CONFIG_LOGGING_HTTP_URI, DEFAULT_LOGGING_HTTP_URI);
    
    COMMIT;
    LOAD_CONFIG;
  END INSTALL_CFG_DEFAULTS;
  
  
  FUNCTION LOADCFG(key IN VARCHAR2, defaultValue IN VARCHAR2) RETURN VARCHAR2 IS
    val VARCHAR2(2000);
  BEGIN
    SELECT TRIM(V) into val FROM TSDB_CONFIG WHERE K = key;
    RETURN val;
    EXCEPTION WHEN OTHERS THEN
      RETURN defaultValue;
  END LOADCFG;

  FUNCTION LOADCFG(key IN VARCHAR2, defaultValue IN NUMBER) RETURN NUMBER IS
    val NUMBER;
  BEGIN
    SELECT TO_NUMBER(TRIM(V)) into val FROM TSDB_CONFIG WHERE K = key;
    RETURN val;
    EXCEPTION WHEN OTHERS THEN
      RETURN defaultValue;
  END LOADCFG;
  
  --===================================================================================================================
  --  Indicates if the passed logging level is enabled: 0: OFF, 1: ERROR, 2: INFO, 3:ALL
  --===================================================================================================================
  FUNCTION IS_LOG_ENABLED(level IN PLS_INTEGER) RETURN BOOLEAN IS
  BEGIN
    IF(loggingLevel=0) THEN RETURN FALSE; END IF;
    RETURN loggingLevel >= level;
  END IS_LOG_ENABLED;
  
  --===================================================================================================================
  --  Returns the cleaned user statistic keys
  --===================================================================================================================
  FUNCTION USERSTATKEYS RETURN VARCHAR2_ARR IS
  BEGIN
    RETURN userStatKeys_;
  END USERSTATKEYS;


  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN NUMBER AS
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := toTime - fromTime;
  BEGIN
    RETURN ROUND(
      (extract(day from delta)*24*60*60*1000) + 
      (extract(hour from delta)*60*60*1000) + 
      (extract(minute from delta)*60*1000) + 
      extract(second from delta)*1000
    ,0);
  END ELAPSEDMS;

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN NUMBER AS
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := toTime - fromTime;
  BEGIN
    RETURN ROUND(
      (extract(day from delta)*24*60*60) + 
      (extract(hour from delta)*60*60) + 
      (extract(minute from delta)*60) + 
      extract(second from delta)
    ,0);
  END ELAPSEDSEC;
   
  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN TIMESTAMP WITH TIME ZONE DEFAULT SYSTIMESTAMP) RETURN NUMBER AS
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := toTime - fromTime;
    ms NUMBER;
  BEGIN
    ms := ROUND(
      (
      (extract(day from delta)*24*60*60) + 
      (extract(hour from delta)*60*60) + 
      (extract(minute from delta)*60) + 
      extract(second from delta)
      ) * 1000
    ,0);
    RETURN ms;
  END ELAPSEDMS;
  

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSTIMESTAMP if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN TIMESTAMP WITH TIME ZONE DEFAULT SYSTIMESTAMP) RETURN NUMBER AS
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := toTime - fromTime;
  BEGIN
    RETURN ROUND(
      (extract(day from delta)*24*60*60) + 
      (extract(hour from delta)*60*60) + 
      (extract(minute from delta)*60) + 
      extract(second from delta)
    ,0);
  END ELAPSEDSEC;
  
  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in milliseconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSDATE if null
  --===================================================================================================================
  FUNCTION ELAPSEDMS(fromTime IN TIMESTAMP, toTime IN DATE DEFAULT SYSDATE) RETURN NUMBER IS
  BEGIN
    RETURN ELAPSEDMS(fromTime, DATETOTIMESTAMP(toTime));
  END ELAPSEDMS;

  --===================================================================================================================
  --  Returns the delta between the fromTime and the toTime in seconds
  --  The fromTime in mandatory
  --  The toTime will default to SYSDATE if null
  --===================================================================================================================
  FUNCTION ELAPSEDSEC(fromTime IN TIMESTAMP, toTime IN DATE DEFAULT SYSDATE) RETURN NUMBER IS
  BEGIN
    RETURN ELAPSEDSEC(fromTime, DATETOTIMESTAMP(toTime));
  END ELAPSEDSEC;
  
  
  --==================================================================================================
  -- Converts the passed date to a timestamp with as high precision as possible.
  -- The passed date defaults to 'SYSDATE'
  -- Additional fractional seconds can be specified to add precision.
  --==================================================================================================
  FUNCTION DATETOTIMESTAMP(dt IN DATE DEFAULT SYSDATE, fs IN INTEGER DEFAULT 0) RETURN TIMESTAMP IS
  BEGIN
    RETURN TO_TIMESTAMP(
      TO_CHAR(dt, 'DD-Mon-RR HH24:MI:SS') || '.' || fs,
      'DD-Mon-RR HH24:MI:SS.FF');
  END DATETOTIMESTAMP;


  --===================================================================================================================
  --  Returns the current time as the number of milliseconds since epoch (like java.lang.System.getCurrentTimeMillis())
  --===================================================================================================================
  FUNCTION CURRENTMS RETURN NUMBER AS
    now TIMESTAMP := SYSTIMESTAMP;
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := now - EPOCH;
  BEGIN  
    RETURN ROUND(
      (extract(day from delta)*24*60*60*1000) + 
      (extract(hour from delta)*60*60*1000) + 
      (extract(minute from delta)*60*1000) + 
      extract(second from delta)*1000
       - (TZOFFSETSECS * 1000)
      + to_number(to_char(sys_extract_utc(now), 'FF3'))
    ,0);
  END CURRENTMS;

  --===================================================================================================================
  --  Returns the current time as the number of seconds since epoch (unix time)
  --===================================================================================================================
  FUNCTION CURRENTSEC RETURN NUMBER AS
    delta CONSTANT INTERVAL DAY (9) TO SECOND  := SYSTIMESTAMP - EPOCH;
  BEGIN  
    RETURN ROUND(
      (extract(day from delta)*24*60*60) + 
      (extract(hour from delta)*60*60) + 
      (extract(minute from delta)*60) + 
      extract(second from delta) - TZOFFSETSECS
    ,0);
  END CURRENTSEC;
  
  --===================================================================================================================
  --  Determines how many digits are in the passed number
  --===================================================================================================================
  FUNCTION DIGITS(n IN NUMBER DEFAULT 0) RETURN INT AS
    rn NUMBER := ROUND(n,0);
  BEGIN
    IF(rn=0) THEN 
      RETURN 1; 
    ELSE
      RETURN ROUND(LOG(10,rn),0);
    END IF;
  END DIGITS;
  
  --===================================================================================================================
  --  Determines if the passed number is most likely SECONDS or MILLISECONDS and returns the value in MILLISECONDS
  --===================================================================================================================
  FUNCTION TOMS(n IN NUMBER DEFAULT 0) RETURN NUMBER IS
  BEGIN
    IF(n=0) THEN
      RETURN 0;      
    ELSIF(DIGITS(n) < 13) THEN
      -- SECONDS
      RETURN n * 1000;
    ELSE
      -- MILLISECONDS
      RETURN n;
    END IF;
  END TOMS;
  
  --===================================================================================================================
  --  Determines if the passed number is most likely SECONDS or MILLISECONDS and returns the value in SECONDS
  --===================================================================================================================
  FUNCTION TOSEC(n IN NUMBER DEFAULT 0) RETURN NUMBER IS
  BEGIN
    IF(n=0) THEN
      RETURN 0;      
    ELSIF(DIGITS(n) < 13) THEN
      -- SECONDS
      RETURN n;
    ELSE
      -- MILLISECONDS
      RETURN n/1000;
    END IF;
  END TOSEC;
  
  
  --===================================================================================================================
  --  Returns the sid for the current session
  --===================================================================================================================
  FUNCTION SESSION_SID RETURN NUMBER IS
  BEGIN
    RETURN sid;
  END SESSION_SID;
  
  --===================================================================================================================
  --  Returns the database name
  --===================================================================================================================
  FUNCTION DB_NAME RETURN VARCHAR2 IS
  BEGIN
    RETURN dbName;
  END DB_NAME;

  --===================================================================================================================
  --  Returns the database host
  --===================================================================================================================
  FUNCTION DB_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN dbHost;
  END DB_HOST;

  --===================================================================================================================
  --  Returns the database IP address
  --===================================================================================================================
  FUNCTION DB_IP RETURN VARCHAR2 IS
  BEGIN
    RETURN dbIp;
  END DB_IP;

  --===================================================================================================================
  --  Returns the host name of the connecting client this session was initiated by
  --===================================================================================================================
  FUNCTION CLIENT_HOST RETURN VARCHAR2 IS
  BEGIN
    RETURN clientHost;
  END CLIENT_HOST;
  
  --===================================================================================================================
  --  Returns the db user name for this session
  --===================================================================================================================
  FUNCTION DB_USER RETURN VARCHAR2 IS
  BEGIN
    RETURN dbUser;
  END DB_USER;
  
  
  --===================================================================================================================
  --  Cleans the passed string to remove whitespace, lowercase and illegal punctuation
  --===================================================================================================================
  FUNCTION CLEAN(str IN VARCHAR2, repl IN CHAR DEFAULT '.') RETURN VARCHAR2 IS
    cs VARCHAR2(360);
    replString CONSTANT VARCHAR2(2) := repl || repl;
  BEGIN
    IF(str IS NULL) THEN 
      RAISE_APPLICATION_ERROR(-20101, 'The passed varchar was null');
    END IF;
    --cs := TRANSLATE(RTRIM(LTRIM(LOWER(str))), ' /*().+%', '__');
    --cs := REPLACE(TRANSLATE(REPLACE(REPLACE(TRIM(LOWER(str)), 'i/o', 'io'), '(', ''), ' /*()+%:''', replString), '_-_', '');
    cs := REGEXP_REPLACE(REGEXP_REPLACE(LOWER(str), '[-''/*()+%:]', ''), '\s+', repl);
    IF(cs IS NULL) THEN 
      RAISE_APPLICATION_ERROR(-20101, 'The passed varchar was empty');
    END IF;    
    RETURN cs;
  END CLEAN;
  
  --===================================================================================================================
  --  Cleans the passed string to remove whitespace, lowercase and illegal punctuation
  --===================================================================================================================
  PROCEDURE CLEAN(str IN OUT NOCOPY VARCHAR2) IS
  BEGIN
    str := CLEAN(str);
  END CLEAN;
  
  --===================================================================================================================
  --  [Re]Loads the configuration
  --===================================================================================================================
  PROCEDURE LOAD_CONFIG IS
  BEGIN
    tracingType := LOADCFG(CONFIG_TRACING_TYPE, DEFAULT_TRACING_TYPE);
    httpTracerHost := LOADCFG(CONFIG_TRACING_HTTP_HOST, DEFAULT_TRACING_HTTP_HOST);
    httpTracerPort := LOADCFG(CONFIG_TRACING_HTTP_PORT, DEFAULT_TRACING_HTTP_PORT);
    httpTracerUri := LOADCFG(CONFIG_TRACING_HTTP_URI, DEFAULT_TRACING_HTTP_URI);  
    telnetTracerHost := LOADCFG(CONFIG_TRACING_TELNET_HOST, DEFAULT_TRACING_TELNET_HOST);
    telnetTracerPort := LOADCFG(CONFIG_TRACING_TELNET_PORT, DEFAULT_TRACING_TELNET_PORT);    
    loggingType := LOADCFG(CONFIG_LOGGING_TYPE, DEFAULT_LOGGING_TYPE);
    loggingLevel := LOADCFG(CONFIG_LOGGING_LEVEL, DEFAULT_LOGGING_LEVEL);
    telnetLoggingHost := LOADCFG(CONFIG_LOGGING_TELNET_HOST, DEFAULT_LOGGING_TELNET_HOST);
    telnetLoggingPort := LOADCFG(CONFIG_LOGGING_TELNET_PORT, DEFAULT_LOGGING_TELNET_PORT);
    loggingTimestampFormat := LOADCFG(CONFIG_LOGGING_TSFORMAT, DEFAULT_LOGGING_TSFORMAT);
    alertLoggingName := LOADCFG(CONFIG_LOGGING_ALERT_NAME, DEFAULT_LOGGING_ALERT_NAME);    
    httpLoggingHost := LOADCFG(CONFIG_LOGGING_HTTP_HOST, DEFAULT_LOGGING_HTTP_HOST);
    httpLoggingPort := LOADCFG(CONFIG_LOGGING_HTTP_PORT, DEFAULT_LOGGING_HTTP_PORT);
    httpLoggingUri := LOADCFG(CONFIG_LOGGING_HTTP_URI, DEFAULT_LOGGING_HTTP_URI);  
    
  END LOAD_CONFIG;
  

  --===================================================================================================================
  --  Initializes the session info for this session
  --===================================================================================================================
  BEGIN
    SELECT 
      SYS_CONTEXT('USERENV', 'SID'),
      SYS_CONTEXT('USERENV', 'DB_NAME'),
      SYS_CONTEXT('USERENV', 'SERVER_HOST'),
      SYS_CONTEXT('USERENV', 'IP_ADDRESS'),
      SYS_CONTEXT('USERENV', 'HOST'),
      USER
      INTO sid, dbName, dbHost, dbIp, clientHost, dbUser FROM DUAL;    
    LOAD_CONFIG;
END TSDB_UTIL;

/

BEGIN
  TSDB_UTIL.INSTALL_CFG_DEFAULTS;
END;

/
