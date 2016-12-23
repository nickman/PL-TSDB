--------------------------------------------------------
--  DDL for Package Body TSDB_UTIL
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE BODY "TSDB_UTIL" AS

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
  --  The config entries
  --===================================================================================================================
--  -- Config map type
--  TYPE CONFIG_MAP IS TABLE OF TSDB_CONFIG.V%TYPE INDEX BY TSDB_CONFIG.K%TYPE;
--  -- All the entries in a map
--  cfgMap CONFIG_MAP;
  -- Trace Type Key
  CONFIG_TRACING_TYPE CONSTANT VARCHAR2(30) := 'TRACING_TYPE';
  -- Default Trace Type
  DEFAULT_TRACING_TYPE CONSTANT VARCHAR2(30) := 'HTTP';
  -- HTTP/JSON Tracer Host Key
  CONFIG_TRACING_HTTP_HOST CONSTANT VARCHAR2(30) := 'TRACING_HTTP_HOST';
  -- Default HTTP/JSON Tracer Host
  DEFAULT_TRACING_HTTP_HOST CONSTANT VARCHAR2(30) := 'localhost';
  -- HTTP/JSON Tracer Port Key
  CONFIG_TRACING_HTTP_PORT CONSTANT VARCHAR2(30) := 'TRACING_HTTP_PORT';
  -- Default HTTP/JSON Tracer Port
  DEFAULT_TRACING_HTTP_PORT CONSTANT INT := 4242;
  -- HTTP/JSON Tracer URI
  CONFIG_TRACING_HTTP_URI CONSTANT VARCHAR2(30) := 'TRACING_HTTP_URI';
  -- Default HTTP/JSON Tracer URI
  DEFAULT_TRACING_HTTP_URI CONSTANT VARCHAR2(30) := '/api/put';
  
  
  -- Configured Trace Type
  tracingType VARCHAR2(2000);
  -- Configured Trace HTTP/JSON Tracer Host
  httpTracerHost VARCHAR2(2000);
  -- Configured Trace HTTP/JSON Tracer Port
  httpTracerPort INT;
  -- Configured Trace HTTP/JSON Tracer URI
  httpTracerUri VARCHAR2(2000);
  
  
  /*
    tracingType := LOADCFG(CONFIG_TRACING_TYPE, DEFAULT_TRACING_TYPE);
    httpTracerHost := LOADCFG(CONFIG_TRACING_HTTP_HOST, DEFAULT_TRACING_HTTP_HOST);
    httpTracerPort := LOADCFG(CONFIG_TRACING_HTTP_PORT, DEFAULT_TRACING_HTTP_PORT);
    httpTracerUri := LOADCFG(CONFIG_TRACING_HTTP_URI, DEFAULT_TRACING_HTTP_URI);
  */
  
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
  
--declare
--  fromtime TIMESTAMP := to_timestamp('22-DEC-16 04.51.19.476673000 PM', 'DD-MM-RR HH.MI.SS.FF AM');
--  totime TIMESTAMP := to_timestamp('22-DEC-16 04.51.24.491343 PM', 'DD-MM-RR HH.MI.SS.FF AM');
--  delta CONSTANT INTERVAL DAY (9) TO SECOND  := totime - fromtime;
--  elapsed NUMBER;
--begin
--  DBMS_OUTPUT.PUT_LINE('INTERVAL:' || delta);
--  elapsed := TSDB_UTIL.ELAPSEDMS(fromtime, totime);
----  elapsed :=   
----  ROUND(
----    (extract(day from delta)*24*60*60*1000) + 
----    (extract(hour from delta)*60*60*1000) + 
----    (extract(minute from delta)*60*1000) + 
----    extract(second from delta)*1000
----  ,0);
--  DBMS_OUTPUT.PUT_LINE('ELAPSED:' || elapsed);
--end;  

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
  FUNCTION CLEAN(str IN VARCHAR2) RETURN VARCHAR2 IS
    cs VARCHAR2(360);
  BEGIN
    IF(str IS NULL) THEN 
      RAISE_APPLICATION_ERROR(-20101, 'The passed varchar was null');
    END IF;
    --cs := TRANSLATE(RTRIM(LTRIM(LOWER(str))), ' /*().+%', '__');
    cs := TRANSLATE(TRIM(LOWER(str)), ' /*()+%', '__');
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
