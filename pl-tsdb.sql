--------------------------------------------------------
--  File created - Thursday-December-22-2016   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Type METRIC
--------------------------------------------------------

  CREATE OR REPLACE TYPE "METRIC" FORCE AS OBJECT (
  -- The metric name
  METRICNAME VARCHAR2(100),
  -- The metric tags
  TAGS TAGPAIR_ARR,
  -- The effective timestamp of this metric instance
  TSTAMP NUMBER,
  -- The effective value of this metric instance
  VALUE NUMBER,
  -- Start time of a measurement on this metric  
  START_TIME TIMESTAMP(9),  
  -- The elapsed time of a measurement on this metric, if the value is not the elapsed time
  TIMING NUMBER,
  -- Adds a new tagpair to this metric
  MEMBER FUNCTION PUSHTAG(tag IN TAGPAIR) RETURN METRIC,
  -- Adds a new tagpair to this metric
  MEMBER FUNCTION PUSHTAG(k IN VARCHAR2, v IN VARCHAR2) RETURN METRIC,  
  -- Pops the specified number of tagpairs from this metric, defaulting to 1 
  MEMBER FUNCTION POPTAG(tagCnt IN INT DEFAULT 1) RETURN METRIC,
  -- Clears all tags
  MEMBER FUNCTION CLEARTAGS RETURN METRIC,
  -- Sets the value of this metric
  MEMBER FUNCTION VAL(v IN NUMBER) RETURN METRIC,
  -- Sets the effective timestamp of this metric, in milliseconds if you know what's best
  MEMBER FUNCTION TS(ts IN NUMBER) RETURN METRIC,
  -- Starts a new timing at the specified timestamp, defaulting to SYSTIMESTAMP
  MEMBER FUNCTION OPEN(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN METRIC,
  -- Completes a timing computing the elapsed time from the passed timestamp (default SYSTIMESTAMP)
  -- If the passed value is null, the metric value is the elapsed time in ms.
  MEMBER FUNCTION CLOSE(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP, mvalue IN NUMBER DEFAULT NULL) RETURN METRIC,
  -- Returns this metric as a JSON string with a ms timestamp
  MEMBER FUNCTION JSONMS(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2,
  -- Returns this metric as a JSON string with a sec timestamp
  MEMBER FUNCTION JSONSEC(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2,  
  -- Returns this metric as a telnet put command string with a ms timestamp
  MEMBER FUNCTION PUTMS(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2,
  -- Returns this metric as a telnet put command string with a sec timestamp
  MEMBER FUNCTION PUTSEC(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2,  
  -- Adds the default metrics for host and app
  MEMBER FUNCTION HOSTAPPTAGS RETURN METRIC,
  -- Updates this metric, setting the value to passed-metric.VALUE - this-metric.VALUE and setting the timestamp. Returns this metric.
  MEMBER FUNCTION DELTA(met_ric IN METRIC, ts IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN METRIC,  
  -- Updates this metric, setting the value to passed-metric.VALUE - this-metric.VALUE and setting the timestamp.
  MEMBER PROCEDURE DELTA(met_ric IN METRIC, ts IN TIMESTAMP DEFAULT SYSTIMESTAMP),  
  -- Sleeps the specified number of seconds and returns this metric
  MEMBER FUNCTION SLEEP(secs IN NUMBER) RETURN METRIC,
  -- Returns the index of the tag pair with the specified key, or -1 if not found
  MEMBER FUNCTION INDEXOFKEY(key IN VARCHAR2) RETURN INT,
  -- Determines if this metric has a tag with the specified key
  MEMBER FUNCTION HASTAGKEY(key IN VARCHAR2) RETURN BOOLEAN,
  -- Sorts this metric's tags
  MEMBER FUNCTION SORT RETURN METRIC,
  
  -- Generates a unique key for this metric made up of the metric name and tag keys concatenated together  
  MEMBER FUNCTION METRICKEY RETURN VARCHAR2,
  -- Parameterized metric renderer
    -- met_ric: The metric to render
    -- ts: The optional timestamp to use to trace. If null, uses current time. If supplied, should be time since epoch in seconds or milliseconds
    -- tsInMs: If ts is not supplied, indicates if the current time is rendered in ms (true) or seconds (false)
    -- asJson: If true, renders in JSON, otherwise renders as a telnet put  
  STATIC FUNCTION RENDER(met_ric IN OUT NOCOPY METRIC, ts IN NUMBER DEFAULT NULL, tsInMs IN BOOLEAN DEFAULT TRUE, asJson IN BOOLEAN DEFAULT TRUE) RETURN VARCHAR2,
  -- Sorts a TAGPAIR array
  STATIC FUNCTION SORT(tags IN TAGPAIR_ARR) RETURN TAGPAIR_ARR,
  -- Parses the passed name in the form '<metric-name>:<key1>=<value1>,....<keyN>=<valueN>' and creates a metric
  STATIC FUNCTION PARSEMETRIC(name IN VARCHAR2) RETURN METRIC,
  -- Creates a new Metric. The name is mandatory, the tags default to an empty array if null
  CONSTRUCTOR FUNCTION METRIC(name IN VARCHAR2, tags IN TAGPAIR_ARR DEFAULT TAGPAIR_ARR()) RETURN SELF AS RESULT
);
/
CREATE OR REPLACE TYPE BODY "METRIC" AS

  -- Adds a new tagpair to this metric
  MEMBER FUNCTION PUSHTAG(tag IN TAGPAIR) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    RETURN me.PUSHTAG(tag.K, tag.V);
  END PUSHTAG;
  
  -- Adds a new tagpair to this metric
  MEMBER FUNCTION PUSHTAG(k IN VARCHAR2, v IN VARCHAR2) RETURN METRIC AS
    me METRIC := SELF;
    key CONSTANT VARCHAR2(100) := TSDB_UTIL.CLEAN(k);
    cnt INT := -1;
  BEGIN
    cnt := INDEXOFKEY(key);
    if(cnt=-1) THEN
      me.TAGS.extend();
      me.TAGS(TAGS.COUNT + 1) := TAGPAIR(TSDB_UTIL.CLEAN(k),TSDB_UTIL.CLEAN(v));
    ELSE
      me.TAGS(cnt).V := TSDB_UTIL.CLEAN(v);
    END IF;
    RETURN me;
  END PUSHTAG;
  
  
  -- Pops the specified number of tagpairs from this metric, defaulting to 1 
  MEMBER FUNCTION POPTAG(tagCnt IN INT DEFAULT 1) RETURN METRIC AS
    me METRIC := SELF;
    sz CONSTANT INT := me.TAGS.COUNT;
  BEGIN
    IF(TAGS.COUNT > 0) THEN
      FOR i in 1..sz LOOP
        --me.TAGS.DELETE(TAGS.COUNT);
        me.TAGS.TRIM();
        EXIT WHEN me.TAGS.COUNT = 0;
      END LOOP;
    END IF;
    RETURN me;
  END POPTAG;
  
    -- Clears all tags
  MEMBER FUNCTION CLEARTAGS RETURN METRIC  AS
    me METRIC := SELF;    
  BEGIN
    me.TAGS.DELETE();
    RETURN me;
  END CLEARTAGS;
  
  -- Sets the value for this metric
  MEMBER FUNCTION VAL(v IN NUMBER) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me.VALUE := v;
    RETURN me;
  END VAL;
  
  -- Sets the effective timestamp of this metric
  MEMBER FUNCTION TS(ts IN NUMBER) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me.TSTAMP:= ts;
    RETURN me;
  END TS;  
  
  
  -- Starts a new timing at the specified timestamp, defaulting to SYSTIMESTAMP
  MEMBER FUNCTION OPEN(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me.START_TIME := ts;
    LOGGING.tcplog('METRIC OPEN: st: [' || me.START_TIME || ']' );
    RETURN me;
  END OPEN;
  
  -- Completes a timing computing the elapsed time from the passed timestamp (default SYSTIMESTAMP)
  -- If the passed value is null, the metric value is the elapsed time in ms.
  MEMBER FUNCTION CLOSE(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP, mvalue IN NUMBER DEFAULT NULL) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    IF(mvalue IS NULL) THEN
      me.VALUE := TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts);
      LOGGING.tcplog('METRIC CLOSE: value set: [' || me.VALUE || '], calced elapsed: [' || TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts) || '], ts: [' || ts || '], st: [' || me.START_TIME || ']' );
    ELSE
      me.VALUE := mvalue;
      me.TIMING := TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts);
    END IF;    
    RETURN me;
  END CLOSE;
  
  -- Updates this metric, setting the value to passed-metric.VALUE - this-metric.VALUE and setting the timestamp.
  -- Returns this metric
  MEMBER FUNCTION DELTA(met_ric IN METRIC, ts IN TIMESTAMP DEFAULT SYSTIMESTAMP) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me.VALUE := met_ric.VALUE - me.VALUE;
    me.TSTAMP := TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, ts);
    RETURN me;
  END DELTA;
  
  -- Updates this metric, setting the value to passed-metric.VALUE - this-metric.VALUE and setting the timestamp.
  MEMBER PROCEDURE DELTA(met_ric IN METRIC, ts IN TIMESTAMP DEFAULT SYSTIMESTAMP) AS
  BEGIN
    VALUE := met_ric.VALUE - VALUE;
    TSTAMP := TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, ts);
  END DELTA;
  
  
    -- Adds the default metrics for host and app
  MEMBER FUNCTION HOSTAPPTAGS RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me := me.PUSHTAG(TAGPAIR('host', TSDB_UTIL.DB_HOST));
    me := me.PUSHTAG(TAGPAIR('app', TSDB_UTIL.DB_NAME));
    me := me.PUSHTAG(TAGPAIR('user', TSDB_UTIL.DB_USER));
    RETURN me;
  END HOSTAPPTAGS;
  
  -- Sleeps the specified number of seconds and returns this metric
  MEMBER FUNCTION SLEEP(secs IN NUMBER) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    DBMS_LOCK.SLEEP(secs);
    RETURN me;
  END SLEEP;
  
    -- Sorts this metric's tags
  MEMBER FUNCTION SORT RETURN METRIC AS
    me METRIC := SELF;
    ctags TAGPAIR_ARR;
  BEGIN
    SELECT VALUE(T) BULK COLLECT INTO ctags FROM TABLE(me.tags) T ORDER BY VALUE(T);
    me.tags := ctags;
    RETURN me;
  END SORT;
  
  
  -- Parametersized metric renderer
    -- met_ric: The metric to render
    -- ts: The optional timestamp to use to trace. If null, uses current time. If supplied, should be time since epoch in seconds or milliseconds
    -- tsInMs: If ts is not supplied, indicates if the current time is rendered in ms (true) or seconds (false)
    -- asJson: If true, renders in JSON, otherwise renders as a telnet put
  STATIC FUNCTION RENDER(met_ric IN OUT NOCOPY METRIC, ts IN NUMBER DEFAULT NULL, tsInMs IN BOOLEAN DEFAULT TRUE, asJson IN BOOLEAN DEFAULT TRUE) RETURN VARCHAR2 AS
    met VARCHAR2(1000);
    delim CHAR(1);
    endchar VARCHAR2(3);
    sz PLS_INTEGER := met_ric.TAGS.COUNT;
  BEGIN
    met_ric.TAGS := METRIC.SORT(met_ric.TAGS);
    IF(ts IS NULL) THEN
      IF(tsInMs) THEN
        met_ric.TSTAMP := TSDB_UTIL.CURRENTMS();
      ELSE 
        met_ric.TSTAMP := TSDB_UTIL.CURRENTSEC();
      END IF;
    ELSE
      met_ric.TSTAMP := ts;
    END IF;
  
    IF(asJson) THEN
      met := '{"metric":"' || met_ric.METRICNAME || '","value":' || met_ric.VALUE || ',"timestamp":' || met_ric.TSTAMP || ',"tags":{';
      delim := ',';
      endchar := '}}';
    ELSE
      -- put $metric $now $value dc=$DC host=$HOST
      met := 'put ' || met_ric.METRICNAME || ' ' || met_ric.TSTAMP || ' ' || met_ric.VALUE || ' ';
      delim := ' ';
      endchar := TSDB_UTIL.EOL;
    END IF;

    FOR i in 1..sz LOOP
      IF(i > 1) THEN
        met := met || delim;
      END IF;
      IF(asJson) THEN
        met := met || met_ric.TAGS(i).JSON();
      ELSE
        met := met || met_ric.TAGS(i).PUT();
      END IF;
    END LOOP;
    met := met || endchar;
    RETURN met;
  END RENDER;
  
    -- Returns this metric as a JSON string with a ms timestamp
  MEMBER FUNCTION JSONMS(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2 AS
    me METRIC := SELF;
  BEGIN
    RETURN METRIC.RENDER(me, ts, TRUE, TRUE);
  END JSONMS;
  
  -- Returns this metric as a JSON string with a sec timestamp
  MEMBER FUNCTION JSONSEC(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2  AS
    me METRIC := SELF;
  BEGIN
    RETURN METRIC.RENDER(me, ts, FALSE, TRUE);
  END JSONSEC;
  
  -- Returns this metric as a telnet put command string with a ms timestamp
  MEMBER FUNCTION PUTMS(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2 AS
    me METRIC := SELF;
  BEGIN
    RETURN METRIC.RENDER(me, ts, TRUE, FALSE);
  END PUTMS;
  
  -- Returns this metric as a telnet put command string with a sec timestamp
  MEMBER FUNCTION PUTSEC(ts IN NUMBER DEFAULT NULL) RETURN VARCHAR2 AS
    me METRIC := SELF;
  BEGIN
    RETURN METRIC.RENDER(me, ts, FALSE, FALSE);
  END PUTSEC;
  
  -- Sorts a TAGPAIR array
  STATIC FUNCTION SORT(tags IN TAGPAIR_ARR) RETURN TAGPAIR_ARR AS
    tparr TAGPAIR_ARR;
  BEGIN
    SELECT VALUE(T) BULK COLLECT INTO tparr FROM TABLE(tags) T ORDER BY VALUE(T);
    RETURN tparr;
  END SORT;

  -- Generates a unique key for this metric made up of the metric name and tag keys concatenated together
  MEMBER FUNCTION METRICKEY RETURN VARCHAR2 AS
    key VARCHAR2(320);
  BEGIN
    SELECT METRICNAME || ':' || LISTAGG(T.K || '=' || T.V, ',') WITHIN GROUP (ORDER BY VALUE(T)) INTO key FROM TABLE(TAGS) T;
    RETURN key;
  END METRICKEY;
  
    -- Returns the index of the tag pair with the specified key, or -1 if not found
  MEMBER FUNCTION INDEXOFKEY(key IN VARCHAR2) RETURN INT IS
    ckey VARCHAR2(100) := TSDB_UTIL.CLEAN(key);
    cnt INT := 0;  
  BEGIN
    SELECT R INTO cnt FROM (SELECT ROWNUM R, T.K KEY FROM TABLE(TAGS) T) X WHERE X.KEY = ckey;
    RETURN cnt;
    EXCEPTION WHEN NO_DATA_FOUND THEN
      RETURN -1;
  END INDEXOFKEY;
  
  -- Determines if this metric has a tag with the specified key
  MEMBER FUNCTION HASTAGKEY(key IN VARCHAR2) RETURN BOOLEAN IS
    ckey VARCHAR2(100) := TSDB_UTIL.CLEAN(key);
    cnt INT := 0;
  BEGIN
    SELECT COUNT(*) INTO cnt FROM TABLE(TAGS) WHERE K = ckey;
    IF(cnt=0) THEN
      RETURN FALSE;    
    END IF;
    RETURN TRUE;
  END HASTAGKEY;
  

  
  -- Parses the passed name in the form '<metric-name>:<key1>=<value1>,....<keyN>=<valueN>' and creates a metric
  STATIC FUNCTION PARSEMETRIC(name IN VARCHAR2) RETURN METRIC IS
    x PLS_INTEGER := 0;
    tname VARCHAR2(1000) := RTRIM(LTRIM(name));
    met METRIC;
  BEGIN  
    x := INSTR(name, ':');
    met := METRIC(SUBSTR(tname, 0, x-1));
    tname := SUBSTR(tname, x+1);
    x := INSTR(tname, ',');
    WHILE(x != 0) LOOP
      met := met.PUSHTAG(TAGPAIR(SUBSTR(tname, 0, x-1)));
      tname := SUBSTR(tname, x+1);
      x := INSTR(tname, ',');
    END LOOP;
    IF(length(tname) > 3) THEN
      met := met.PUSHTAG(TAGPAIR(tname));
    END IF;
    return met.HOSTAPPTAGS();
    EXCEPTION WHEN OTHERS THEN
      DECLARE
      errm VARCHAR2(1000) := SQLERRM();
      BEGIN
        RAISE_APPLICATION_ERROR(-20101, 'Could not parse metric [' || name || ']: ' || errm);
      END;    
  END PARSEMETRIC;
  

  
  
  -- Creates a new Metric. The name is mandatory, the tags default to an empty array if null
  CONSTRUCTOR FUNCTION METRIC(name IN VARCHAR2, tags IN TAGPAIR_ARR DEFAULT TAGPAIR_ARR()) RETURN SELF AS RESULT AS
  BEGIN
    IF(name IS NULL) THEN
      RAISE_APPLICATION_ERROR(-20101, 'Metric name was null');
    END IF;
    
    METRICNAME := TSDB_UTIL.CLEAN(name);
    IF(METRICNAME IS NULL) THEN
      RAISE_APPLICATION_ERROR(-20101, 'Metric name was null');
    END IF;
    
    SELF.TAGS := tags;
    
    RETURN;
  END;
END;

/
--------------------------------------------------------
--  DDL for Type METRIC_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE "METRIC_ARR" as table of metric

/
--------------------------------------------------------
--  DDL for Type TAGPAIR
--------------------------------------------------------

  CREATE OR REPLACE TYPE "TAGPAIR" force as object ( 
  K VARCHAR(60),
  V VARCHAR(60),
  MEMBER FUNCTION JSON RETURN VARCHAR2,
  MEMBER FUNCTION PUT RETURN VARCHAR2,
  CONSTRUCTOR FUNCTION TAGPAIR(K IN VARCHAR, V IN VARCHAR) RETURN SELF AS RESULT,
  CONSTRUCTOR FUNCTION TAGPAIR RETURN SELF AS RESULT,
    -- Parse a tag pair in the form 'K=V'
  CONSTRUCTOR FUNCTION TAGPAIR(name IN VARCHAR2) RETURN SELF AS RESULT,

  ORDER MEMBER FUNCTION MATCH(tp IN TAGPAIR) RETURN INTEGER
);
/
CREATE OR REPLACE TYPE BODY "TAGPAIR" as

  member function json return varchar2 as
    begin    
      return '"' || K || '":"' || V || '"';
    end json;
    
  MEMBER FUNCTION PUT RETURN VARCHAR2 IS
  BEGIN
    RETURN K || '=' || V;
  END PUT;
  
      -- Parse a tag pair in the form 'K=V'
  CONSTRUCTOR FUNCTION TAGPAIR(name IN VARCHAR2) RETURN SELF AS RESULT IS
    tname VARCHAR2(1000) := RTRIM(LTRIM(name));
    x PLS_INTEGER := INSTR(tname, '=');    
    BEGIN
      SELF := TAGPAIR(SUBSTR(tname, 0, x-1), SUBSTR(tname, x+1));
    RETURN;
  END;
  
  constructor function tagpair(k in varchar, v in varchar) return self as result as
    begin
      IF(k IS NULL OR RTRIM(LTRIM(k)) IS NULL) THEN
        RAISE_APPLICATION_ERROR(-24000, 'The tag key was null or empty');
      END IF;
      IF(v IS NULL OR RTRIM(LTRIM(v)) IS NULL) THEN
        RAISE_APPLICATION_ERROR(-24000, 'The tag value was null or empty');
      END IF;
      SELF.k := TSDB_UTIL.CLEAN(k); 
      SELF.v := TSDB_UTIL.CLEAN(v);
      return;
    end tagpair;
  
  CONSTRUCTOR FUNCTION TAGPAIR RETURN SELF AS RESULT AS
  BEGIN
    RAISE_APPLICATION_ERROR(-20101, 'Must provide key and value for tagpair. Please use constructor function tagpair(k in varchar, v in varchar)');
  END;

  ORDER MEMBER FUNCTION MATCH(tp IN TAGPAIR) RETURN INTEGER IS
    me TAGPAIR := SELF;
  BEGIN
    IF(tp IS NULL) THEN RETURN NULL; END IF;
    --
    IF(me.K = 'host') THEN
      IF(tp.K = 'host') THEN
        RETURN 0;
      ELSE
        RETURN -1;
      END IF;
    ELSIF(me.K = 'app') THEN
      IF(tp.K = 'app') THEN
        RETURN 0;
      ELSE
        RETURN -1;
      END IF;
    ELSE
      IF(tp.K = 'host' OR tp.K = 'app') THEN
        RETURN 1;
      ELSIF(me.K = tp.K) THEN
        RETURN 0;
      ELSIF(me.K < tp.K) THEN
        RETURN -1;
      ELSE
        RETURN 0;
      END IF;    
    END IF;
  END;



end;

/
--------------------------------------------------------
--  DDL for Type TAGPAIR_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE "TAGPAIR_ARR" IS  TABLE OF TAGPAIR;

/
--------------------------------------------------------
--  DDL for Type VARCHAR2_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE "VARCHAR2_ARR" FORCE IS TABLE OF VARCHAR2(200);

/
--------------------------------------------------------
--  DDL for Package TSDB_UTIL
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE "TSDB_UTIL" AS 

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
  FUNCTION CLEAN(str IN VARCHAR2) RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Cleans the passed string to remove whitespace, lowercase and illegal punctuation
  --===================================================================================================================
  PROCEDURE CLEAN(str IN OUT NOCOPY VARCHAR2);
  
  
END TSDB_UTIL;

/
--------------------------------------------------------
--  DDL for Package TSDB_TRACER
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE "TSDB_TRACER" authid definer AS 
  /* Type defining a map of metrics keyed by the metric key */
  TYPE XMETRIC_ARR IS TABLE OF METRIC INDEX BY VARCHAR2(360);
  /* Type defining a stack of metric arrays for handling nested delta measurements */
  TYPE XMETRIC_ARR_STACK IS TABLE OF XMETRIC_ARR INDEX BY PLS_INTEGER;
  /* Type defining a map of metric arrays keyed by a supplied logical name */
  TYPE XMETRIC_NAMED_ARR IS TABLE OF XMETRIC_ARR INDEX BY VARCHAR2(360);
  
  /* Ref Cursor Def */
  TYPE RCUR IS REF CURSOR;
  
  
  /* The max size of a tag key */
  MAX_TAGK CONSTANT PLS_INTEGER := 70;
  /* The max size of a tag value */
  MAX_TAGV CONSTANT PLS_INTEGER := 70;
  /* The max size of a metric name */
  MAX_METRICN CONSTANT PLS_INTEGER := 70;
  
  --==================================================================================
  -- Exception codes
  --==================================================================================
  /* Raised when a metric is added to the stack, or the current stack is cleared but there is no current stack entry */
  no_metric_stack_entry EXCEPTION;
  /* Raised when an invalid depth is provided when adding or updating metrics in the specified stack entry */
  invalid_stack_depth EXCEPTION;
  /* Raised when a TOMETRICS call via SQL or a REF CURSOR returns an unsupported type for the timestamp */
  unsupported_timestamp_type EXCEPTION;
  /* Raised when a timer is started but the timed metric is already in state */
  timer_metric_already_started EXCEPTION;
  /* Raised when a timer is stopped but the timed metric was never started */
  timer_metric_never_started EXCEPTION;
  
    -- EOL Char
  EOL VARCHAR2(2) := '
';

  
  --===================================================================================================================
  --  Returns the user stats as metrics
  --===================================================================================================================
  FUNCTION USERSTATS RETURN METRIC_ARR;
  
  --===================================================================================================================
  --  Starts a timer on a metric built from the passed name.
  --  Returns the metric name.
  --===================================================================================================================
  FUNCTION STARTTIMER(metricName IN VARCHAR2) RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Starts a timer on the passed metric
  --  Returns the metric name.
  --===================================================================================================================
  FUNCTION STARTTIMER(met_ric IN METRIC) RETURN VARCHAR2;
  
  --===================================================================================================================
  --  Starts a timer on a metric built from the passed name.
  --  Returns the metric.
  --===================================================================================================================
  FUNCTION STARTTIMERMETRIC(metricName IN VARCHAR2) RETURN METRIC;
  
  --===================================================================================================================
  --  Starts a timer on the passed metric
  --  Returns the metric.
  --===================================================================================================================
  FUNCTION STARTTIMERMETRIC(met_ric IN METRIC) RETURN METRIC;
  
  
  --===================================================================================================================
  --  Stops the named timer and returns the elapsed time metric.
  --  The metric is removed from state.
  --===================================================================================================================
  FUNCTION STOPTIMER(metricName IN VARCHAR2) RETURN METRIC;
  
  --===================================================================================================================
  --  Stops the timer for the passed metric and returns the elapsed time metric.
  --  The metric is removed from state.
  --===================================================================================================================
  FUNCTION STOPTIMER(met_ric IN METRIC) RETURN METRIC;
  

  --===================================================================================================================
  --  Clears all timers
  --===================================================================================================================
  PROCEDURE CLEARTIMERS;


  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICS(p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR;  
  
  --====================================================================================================
  -- Converts the results from the passed SQL query to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION SQLTOMETRICS(query IN VARCHAR2) RETURN METRIC_ARR;  

  -- Decodes a class number to the class name
  FUNCTION DECODE_CLASS(classNum IN PLS_INTEGER) RETURN VARCHAR2;
  
  -- Add a metric to the metric stack entry at the specified mdepth
  FUNCTION ADD_METRIC(mdepth IN INT, met_ric IN METRIC) RETURN METRIC;
  -- Adds an array of metrics to the metric stack entry at the specified mdepth
  PROCEDURE ADD_METRICS(mdepth IN INT, metrics IN METRIC_ARR);
  -- Adds an array of metrics to the metric stack entry at the specified mdepth, returns the mdepth
  FUNCTION ADD_METRICS(mdepth IN INT, metrics IN METRIC_ARR) RETURN INT;
  
  -- Update a delta metric in the metric stack entry at the specified mdepth
  -- Returns the updated metric
  FUNCTION UPDATE_DELTA_METRIC(mdepth IN INT, met_ric IN METRIC) RETURN METRIC;
  -- Updates an array of delta metrics in the metric stack entry at the specified mdepth
  PROCEDURE UPDATE_DELTA_METRICS(mdepth IN INT, metrics IN METRIC_ARR);
  
--  -- Stacks the passed metrics and starts their elapsed times
--  FUNCTION START_ELAPSED_METRIC(mdepth IN INT, metrics IN METRIC_ARR) RETURN METRIC;
  
  
  
  -- Pops a metric array off the metric stack
  FUNCTION POP RETURN METRIC_ARR;
  
  FUNCTION PIPEX(xarr IN XMETRIC_ARR) RETURN METRIC_ARR PIPELINED;
  
  FUNCTION TOX(arr IN METRIC_ARR) RETURN XMETRIC_ARR;
  
  PROCEDURE DEBUG_UPDATE(mdepth IN INT);
  
  
  -- Trace from a ref cursor
  FUNCTION TRACE(p IN RCUR) RETURN INT;
  
  -- Closes any persistent connections
  PROCEDURE CLOSE_PERSISTENT_CONNS;
  
  -- Clear all metrics
  PROCEDURE CLEARSTACK;
  
  -- Clear all metrics in the current stack and pops the entry
  PROCEDURE CLEAR;

  -- Starts a new metric stack and returns the new depth
  FUNCTION STARTMETRICSTACK RETURN INT;
  
  -- Trace all metrics
  PROCEDURE TRACE(metrics IN METRIC_ARR);
  -- Trace a metric
  PROCEDURE TRACE(metric IN METRIC);
  
  FUNCTION INDIRECT(p IN RCUR) RETURN SYS_REFCURSOR;
  
  -- *******************************************************
  --    Get current XID function
  -- *******************************************************
  FUNCTION CURRENTXID RETURN RAW;  

END TSDB_TRACER;

/
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
END TSDB_UTIL;

/
--------------------------------------------------------
--  DDL for Package Body TSDB_TRACER
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE BODY "TSDB_TRACER" AS

  /* The metric stack */
  metricStack XMETRIC_ARR_STACK;
  /* The current metric stack depth */
  depth PLS_INTEGER := 0;  
  /* The named metrics map */
  namedMetrics XMETRIC_NAMED_ARR;
  /* A map of timer metrics keyed by the metric key */
  timers XMETRIC_ARR;
  
  
  FUNCTION TOMETRICS(p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR IS
  BEGIN
    --FETCH p BULK COLLECT INTO r;
    RETURN REFCURTOMETRICS(p);
  END TOMETRICS;
  
  --===================================================================================================================
  --  Starts a timer on a metric built from the passed name.
  --  Returns the metric name.
  --===================================================================================================================
  FUNCTION STARTTIMER(metricName IN VARCHAR2) RETURN VARCHAR2 IS
    m METRIC;
  BEGIN
    m := METRIC.PARSEMETRIC(metricName);
    RETURN STARTTIMER(m);
  END STARTTIMER;
  
  --===================================================================================================================
  --  Starts a timer on the passed metric
  --  Returns the metric name.
  --===================================================================================================================
  FUNCTION STARTTIMER(met_ric IN METRIC) RETURN VARCHAR2 IS
    mkey VARCHAR2(100) := met_ric.METRICKEY();
    m METRIC := met_ric;
  BEGIN
    IF(timers.EXISTS(mkey)) THEN
      RAISE timer_metric_already_started;
    END IF;
    timers(mkey) := m.OPEN();
    RETURN mkey;
  END STARTTIMER;
  
  --===================================================================================================================
  --  Starts a timer on the passed metric
  --  Returns the metric.
  --===================================================================================================================
  FUNCTION STARTTIMERMETRIC(met_ric IN METRIC) RETURN METRIC IS
    mkey VARCHAR2(100) := met_ric.METRICKEY();
    m METRIC;
  BEGIN
    IF(timers.EXISTS(mkey)) THEN
      RAISE timer_metric_already_started;
    END IF;
    m := met_ric.OPEN();
    timers(mkey) := m;
    RETURN m;
  END STARTTIMERMETRIC;
  
  --===================================================================================================================
  --  Starts a timer on a metric built from the passed name.
  --  Returns the metric.
  --===================================================================================================================
  FUNCTION STARTTIMERMETRIC(metricName IN VARCHAR2) RETURN METRIC IS
    m METRIC;
  BEGIN
    m := METRIC.PARSEMETRIC(metricName);
    RETURN STARTTIMERMETRIC(m);
  END STARTTIMERMETRIC;
  
  
  
  --===================================================================================================================
  --  Stops the named timer and returns the elapsed time metric.
  --  The metric is removed from state.
  --===================================================================================================================
  FUNCTION STOPTIMER(metricName IN VARCHAR2) RETURN METRIC IS
    m METRIC;
  BEGIN
    m := METRIC.PARSEMETRIC(metricName);
    RETURN STOPTIMER(m);
  END STOPTIMER;
  
  --===================================================================================================================
  --  Stops the timer for the passed metric and returns the elapsed time metric.
  --  The metric is removed from state.
  --===================================================================================================================
  FUNCTION STOPTIMER(met_ric IN METRIC) RETURN METRIC IS
    mkey CONSTANT VARCHAR2(100) := met_ric.METRICKEY();
    elapsed METRIC;
  BEGIN
    IF(timers.EXISTS(mkey)) THEN
      elapsed := timers(mkey).CLOSE();
      timers.DELETE(mkey);
    ELSE
      RAISE timer_metric_never_started;
    END IF;
    RETURN elapsed;
  END STOPTIMER;
  
  --===================================================================================================================
  --  Clears all timers
  --===================================================================================================================
  PROCEDURE CLEARTIMERS IS
  BEGIN
    timers.DELETE();
  END CLEARTIMERS;
  
  
  
  

  --===================================================================================================================
  --  Returns the user stats as metrics
  --===================================================================================================================
  FUNCTION USERSTATS RETURN METRIC_ARR IS
    p SYS_REFCURSOR;
  BEGIN
    OPEN p FOR 
      SELECT M.VALUE, TSDB_UTIL.CLEAN(N.NAME) NAME, 'CLASS', TSDB_TRACER.DECODE_CLASS(N.CLASS) CLAZZ
      FROM v$mystat M, v$statname N
      WHERE M.STATISTIC# = N.STATISTIC#
      AND EXISTS (
        SELECT COLUMN_VALUE FROM TABLE(TSDB_UTIL.USERSTATKEYS())
        WHERE COLUMN_VALUE = TSDB_UTIL.CLEAN(N.NAME)
      )
    ;
    RETURN REFCURTOMETRICS(p);
  END USERSTATS;


    -- Decodes a class number to the class name
  FUNCTION DECODE_CLASS(classNum IN PLS_INTEGER) RETURN VARCHAR2 IS
    name VARCHAR2(40);
  BEGIN
    select 
    decode (bitand(  1,classNum),  1,'User ',              '') ||
    decode (bitand(  2,classNum),  2,'Redo ',              '') ||
    decode (bitand(  4,classNum),  4,'Enqueue ',           '') ||
    decode (bitand(  8,classNum),  8,'Cache ',             '') ||
    decode (bitand( 16,classNum), 16,'Parallel Server ',   '') ||
    decode (bitand( 32,classNum), 32,'OS ',                '') ||
    decode (bitand( 64,classNum), 64,'SQL ',               '') ||
    decode (bitand(128,classNum),128,'Debug ',             '') INTO name FROM DUAL;
    RETURN name;
  END DECODE_CLASS;



  -- Add a metric to the metric stack entry at the specified mdepth
  FUNCTION ADD_METRIC(mdepth IN INT, met_ric IN METRIC) RETURN METRIC IS
  BEGIN
    IF(metricStack.EXISTS(mdepth)) THEN
      metricStack(mdepth)(met_ric.METRICKEY()) := met_ric;
    ELSE
      RAISE invalid_stack_depth;
    END IF;
    RETURN met_ric;
  END ADD_METRIC;
  
  -- Adds an array of metrics to the metric stack entry at the specified mdepth
  PROCEDURE ADD_METRICS(mdepth IN INT, metrics IN METRIC_ARR) IS
  BEGIN
    IF(metricStack.EXISTS(mdepth)) THEN
      FOR i IN 1..metrics.COUNT LOOP
        IF(metrics(i) IS NOT NULL) THEN
          metricStack(mdepth)(metrics(i).METRICKEY()) := metrics(i);
        END IF;
      END LOOP;
    ELSE
      RAISE invalid_stack_depth;
    END IF;
  END ADD_METRICS;
  
  -- Adds an array of metrics to the metric stack entry at the specified mdepth, returns the mdepth
  FUNCTION ADD_METRICS(mdepth IN INT, metrics IN METRIC_ARR) RETURN INT IS
  BEGIN
    ADD_METRICS(mdepth, metrics);
    RETURN mdepth;
  END ADD_METRICS;
  
  
  -- Update a delta metric in the metric stack entry at the specified mdepth
  -- Returns the updated metric
  -- If the mdepth is valid, but the metric is not found, it will be ignored and returns null
  FUNCTION UPDATE_DELTA_METRIC(mdepth IN INT, met_ric IN METRIC) RETURN METRIC IS
    key CONSTANT VARCHAR2(1000) := met_ric.METRICKEY();
  BEGIN
    IF(metricStack.EXISTS(mdepth)) THEN
      IF(metricStack(depth).EXISTS(key)) THEN
        RETURN metricStack(depth)(key).DELTA(met_ric);
      END IF;
      RETURN NULL;
    ELSE
      RAISE invalid_stack_depth;
    END IF;
  END UPDATE_DELTA_METRIC;
  
  PROCEDURE UPD(arr IN OUT NOCOPY XMETRIC_ARR, metrics IN METRIC_ARR) IS
    key VARCHAR2(1000);
  BEGIN
      FOR i IN 1..metrics.COUNT LOOP
        IF(metrics(i) IS NOT NULL) THEN
          key := metrics(i).METRICKEY();
          IF(arr.EXISTS(key)) THEN
            arr(key).DELTA(metrics(i));
          END IF;
        END IF;
      END LOOP;     
  END UPD;
  
  FUNCTION TOX(arr IN METRIC_ARR) RETURN XMETRIC_ARR IS
    a XMETRIC_ARR;
  BEGIN
    FOR i IN 1..arr.COUNT LOOP      
      a(i) := arr(i);
    END LOOP;
    RETURN a;
  END TOX;
  
  -- Updates an array of delta metrics in the metric stack entry at the specified mdepth
  PROCEDURE UPDATE_DELTA_METRICS(mdepth IN INT, metrics IN METRIC_ARR) IS
    key VARCHAR2(1000);
    m METRIC;
    arr XMETRIC_ARR;
  BEGIN
    IF(metricStack.EXISTS(mdepth)) THEN
      UPD(metricStack(mdepth), metrics);
    ELSE
      RAISE invalid_stack_depth;
    END IF;  
  END UPDATE_DELTA_METRICS;
  
  
  
  PROCEDURE DEBUG_UPDATE(mdepth IN INT) IS
    key VARCHAR2(1000);
    arr XMETRIC_ARR;
    m METRIC;
  BEGIN
    arr := metricStack(mdepth);
      key := arr.first;
      WHILE (key IS NOT NULL) LOOP
        LOGGING.tcplog('POST:' || arr(key).METRICNAME || ':' || arr(key).VALUE || '......[' || arr(key).TSTAMP || ']');
        key := arr.next(key);
      END LOOP;        
  END DEBUG_UPDATE;
  
  FUNCTION PIPEX(xarr IN XMETRIC_ARR) RETURN METRIC_ARR PIPELINED IS  
  BEGIN
    FOR indx IN xarr.FIRST .. xarr.LAST LOOP
      PIPE ROW (xarr(indx));
    END LOOP;
    RETURN;
  END PIPEX;
  
  -- Pops a metric array off the metric stack
  FUNCTION POP RETURN METRIC_ARR IS
    xarr XMETRIC_ARR;
    arr METRIC_ARR := METRIC_ARR();
    ind PLS_INTEGER := 1;
    key VARCHAR2(360);
  BEGIN
    IF(depth = 0) THEN
      RAISE no_metric_stack_entry;
    END IF;
    xarr := metricStack(depth);
    metricStack.DELETE(depth);
    depth := depth -1;    
--    FOR a IN (SELECT VALUE(T) FROM TABLE(PIPEX(xarr)) T) LOOP
--      NULL;
--    END LOOP;
    --SELECT * BULK COLLECT INTO arr FROM TABLE(TSDB_TRACER.PIPEX(xarr)) T;    
    key := xarr.first;
    WHILE (key IS NOT NULL) LOOP
      arr.EXTEND();
      arr(ind) := xarr(key);
--      LOGGING.tcplog('POPPED [' || arr(ind).PUTMS || ']');
      ind := ind + 1;
      key := xarr.next(key);
    END LOOP;    
    RETURN arr;
  END POP;

  
  
  
  -- Starts a new metric stack and returns the new depth
  FUNCTION STARTMETRICSTACK RETURN INT IS
    st XMETRIC_ARR;
  BEGIN
    depth := depth + 1;
    metricStack(depth) := st;
    return depth;
  END STARTMETRICSTACK;


  
  -- Clears the current metric stack entry
  PROCEDURE CLEAR IS
  BEGIN
    IF(depth = 0) THEN
      RAISE no_metric_stack_entry;
    END IF;
    metricStack.delete(depth);
    depth := depth -1;
  END CLEAR;
  
    -- Clear all metrics
  PROCEDURE CLEARSTACK IS 
  BEGIN
    metricStack.delete();
    depth := 0;
  END CLEARSTACK;

    -- Trace a metric
  PROCEDURE TRACE(metric IN METRIC) IS
  BEGIN
    TRACE(METRIC_ARR(metric));
  END TRACE;
  
  -- Trace all metrics
  PROCEDURE TRACE(metrics IN METRIC_ARR) IS
    content CLOB;
    jsonText VARCHAR2(400);
    now TIMESTAMP(9) := SYSTIMESTAMP;
    req   UTL_HTTP.REQ;
    resp  UTL_HTTP.RESP;    
  BEGIN
    DBMS_LOB.CREATETEMPORARY(content, true, DBMS_LOB.CALL);
    DBMS_LOB.OPEN(content, DBMS_LOB.LOB_READWRITE);
    DBMS_LOB.WRITEAPPEND(content, 1, '[');      
    FOR i IN 1..metrics.COUNT LOOP
      IF(length(content) > 1) THEN
        DBMS_LOB.WRITEAPPEND(content, 1, ',');      
      END IF;
      jsonText := metrics(i).JSONMS();
      DBMS_LOB.WRITEAPPEND(content, length(jsonText), jsonText);      
    END LOOP;
    DBMS_LOB.WRITEAPPEND(content, 1, ']');    
    LOGGING.tcplog(content);
    req := UTL_HTTP.BEGIN_REQUEST (url=> 'http://pdk-pt-cltsdb-05.intcx.net:4242/api/put', method => 'POST');
    --req := UTL_HTTP.BEGIN_REQUEST (url=> 'http://localhost:4242/api/put', method => 'POST');
    --UTL_HTTP.set_persistent_conn_support(req,true);
    UTL_HTTP.SET_HEADER (r      =>  req,
                       name   =>  'Content-Type',
                       value  =>  'application/json;charset=UTF-8');
    UTL_HTTP.SET_HEADER (r      =>   req,
                       name   =>   'Content-Length',
                       value  =>   length(content));
    UTL_HTTP.WRITE_TEXT (r      =>   req,
                       data   =>   content);    
    resp := UTL_HTTP.GET_RESPONSE(req);
    UTL_HTTP.END_RESPONSE(resp);
    EXCEPTION WHEN OTHERS THEN 
        DECLARE
          errm VARCHAR2(200) := SQLERRM();
        BEGIN
          LOGGING.tcplog('PIPE_TRADES ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
        END;
        RAISE;                    
    
    DBMS_LOB.CLOSE(content);
    DBMS_LOB.FREETEMPORARY(content);
  END TRACE;
  
    -- Closes any persistent connections
  PROCEDURE CLOSE_PERSISTENT_CONNS IS
  BEGIN
    UTL_HTTP.CLOSE_PERSISTENT_CONNS(host => 'pdk-pt-cltsdb-05.intcx.net', port => 4242);
  END CLOSE_PERSISTENT_CONNS;

  

  FUNCTION INDIRECT(p IN RCUR) RETURN SYS_REFCURSOR IS
    c SYS_REFCURSOR;
  BEGIN    
    OPEN c FOR SELECT p FROM DUAL;
    --OPEN c FOR p;
    RETURN c;
  END INDIRECT;


  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics.
  -- Only works, so far as I know, in Oracle 12+
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICSINONLY(p IN SYS_REFCURSOR) RETURN METRIC_ARR IS
    pout SYS_REFCURSOR := INDIRECT(p);
  BEGIN
    RETURN REFCURTOMETRICS(pout);
  END REFCURTOMETRICSINONLY;
  
  
  
  --====================================================================================================
  -- Converts the results from the passed opened and parsed cursor number to an array of metrics.
  -- The cursor is closed on completion.
  -- Doc needed
  --====================================================================================================
  FUNCTION CURSORINTTOMETRICS(cursorNum IN OUT NOCOPY NUMBER) RETURN METRIC_ARR IS
    cntr PLS_INTEGER := 0;
    rows_processed INTEGER;    
    desctab  DBMS_SQL.DESC_TAB3;
    colcnt   NUMBER;   
    colnum INT;
    hasTs BOOLEAN;
    tsType PLS_INTEGER := NULL;
    tsTypeName VARCHAR2(200);
    tId PLS_INTEGER := 3;
    tagCount PLS_INTEGER;
    met METRIC := NULL;
    metrics METRIC_ARR := METRIC_ARR();
    metName VARCHAR2(100);
    metValue NUMBER;
    tagK VARCHAR2(100);
    tagV VARCHAR2(100);
    rowsFetched PLS_INTEGER := 0;    
    tsTimestamp TIMESTAMP;
    tsTimestampWTimeZone TIMESTAMP WITH TIME ZONE;
    tsDate DATE;
    tsNumber NUMBER;
  BEGIN
    DBMS_SQL.DESCRIBE_COLUMNS3(cursorNum, colcnt, desctab);
    IF(colcnt < 2) THEN
      RETURN metrics;
    END IF;
    hasTs := MOD(colcnt, 2) != 0;
    IF(hasTs) THEN
      tagCount := (colcnt - 3) / 2;
    ELSE
      tagCount := (colcnt - 2) / 2;
    END IF;
    DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, metValue); 
    DBMS_SQL.DEFINE_COLUMN(cursorNum, 2, metName, 100); 
    IF(hasTs) THEN
      tsType := desctab(colcnt).col_type;
      tsTypeName := desctab(colcnt).col_type_name;
      LOGGING.tcplog('TSTYPE: [' || tsType || '], TS NAME: [' || tsTypeName || '], COLCNT: [' || colcnt || ']');
      CASE tsType
--        WHEN DBMS_TYPES.TYPECODE_TIMESTAMP THEN
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsTimestamp, DBMS_TYPES.TYPECODE_TIMESTAMP);
--        WHEN 181 THEN
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsTimestampWTimeZone, DBMS_TYPES.TYPECODE_TIMESTAMP_TZ);          
--        WHEN DBMS_TYPES.TYPECODE_DATE THEN
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsDate, DBMS_TYPES.TYPECODE_DATE);
--        WHEN DBMS_TYPES.TYPECODE_NUMBER THEN
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsNumber, DBMS_TYPES.TYPECODE_NUMBER);
        WHEN DBMS_TYPES.TYPECODE_TIMESTAMP THEN
          LOGGING.tcplog('DEFINE TIMESTAMP');
          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt,  tsTimestamp);
        WHEN 181 THEN
          LOGGING.tcplog('DEFINE TIMESTAMP WITH TZ');
          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsTimestampWTimeZone);          
        WHEN DBMS_TYPES.TYPECODE_DATE THEN
          LOGGING.tcplog('DEFINE DATE');
          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsDate);
        WHEN DBMS_TYPES.TYPECODE_NUMBER THEN
          LOGGING.tcplog('DEFINE NUMBER');
          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsNumber);
        ELSE
          RAISE unsupported_timestamp_type;
      END CASE;
    END IF;
    FOR i IN 1..tagCount LOOP
      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
      LOGGING.tcplog('Bound Tag Key [' || i || '] at position [' || tId || ']');
      tId := tId + 1;
      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
      --DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, tagV, 100);
      LOGGING.tcplog('Bound Tag Value [' || i || '] at position [' || tId || ']');
      tId := tId + 1;
    END LOOP;
    tId := 3;
    colnum := desctab.first;
    rowsFetched := DBMS_SQL.EXECUTE(cursorNum);
    LOOP      
      rowsFetched := DBMS_SQL.FETCH_ROWS(cursorNum);
      EXIT WHEN rowsFetched = 0;
      cntr := cntr + 1;        
      DBMS_SQL.COLUMN_VALUE(cursorNum, 1, metValue);        
      DBMS_SQL.COLUMN_VALUE(cursorNum, 2, metName);
      met := METRIC(metName).HOSTAPPTAGS().VAL(metValue);
      FOR i IN 1..tagCount LOOP
        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagK);
        tId := tId + 1;
        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagV);
        tId := tId + 1;
        met := met.PUSHTAG(tagK, tagV);
      END LOOP;
      IF(hasTs) THEN
        CASE tsType
          WHEN DBMS_TYPES.TYPECODE_TIMESTAMP THEN
            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestamp);
            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestamp));
          WHEN 181 THEN
            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestampWTimeZone);
            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestampWTimeZone));            
          WHEN DBMS_TYPES.TYPECODE_DATE THEN
            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsDate);
            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, TSDB_UTIL.DATETOTIMESTAMP(tsDate)));
          WHEN DBMS_TYPES.TYPECODE_NUMBER THEN
            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsNumber);
            met := met.ts(tsNumber);
          END CASE;
      END IF;
      tId := 3;
      metrics.EXTEND();
      metrics(cntr) := met;
      --LOGGING.tcplog(met.JSONMS());
    END LOOP;
    DBMS_SQL.CLOSE_CURSOR(cursorNum);
    
--    EXCEPTION WHEN OTHERS THEN       
--      DECLARE
--        errm VARCHAR2(200) := SQLERRM();
--      BEGIN
--        LOGGING.tcplog('SQLTOMETRICS(REF CUR) ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
--        DBMS_OUTPUT.PUT_LINE('SQLTOMETRICS(REF CUR) ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
--        RAISE;                    
--      END;
      
      --NULL;
      RETURN metrics;
  END CURSORINTTOMETRICS;
  
  
  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICS(p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR IS
    cursorNum NUMBER;
  BEGIN
    cursorNum := DBMS_SQL.TO_CURSOR_NUMBER(p);    
    RETURN CURSORINTTOMETRICS(cursorNum);
    EXCEPTION WHEN OTHERS THEN
      BEGIN
        DBMS_SQL.CLOSE_CURSOR(cursorNum);    
        EXCEPTION WHEN OTHERS THEN NULL;
      END;
      DECLARE
        errm VARCHAR2(200) := SQLERRM();
      BEGIN
        LOGGING.tcplog('REFCURTOMETRICS(REF CUR) ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
      END;
      RAISE;                    
  END REFCURTOMETRICS;
  
  
  
  --====================================================================================================
  -- Converts the results from the passed SQL query to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION SQLTOMETRICS(query IN VARCHAR2) RETURN METRIC_ARR IS
    cursorNum NUMBER;
  BEGIN
    cursorNum := dbms_sql.open_cursor();
    DBMS_SQL.PARSE(cursorNum, query, DBMS_SQL.NATIVE);     
    RETURN CURSORINTTOMETRICS(cursorNum);
--    EXCEPTION WHEN OTHERS THEN
--      BEGIN
--        DBMS_SQL.CLOSE_CURSOR(cursorNum);    
--        EXCEPTION WHEN OTHERS THEN NULL;
--      END;
--      DECLARE
--        errm VARCHAR2(200) := SQLERRM();
--      BEGIN
--        LOGGING.tcplog('SQLTOMETRICS(REF CUR) ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
--      END;
--      RAISE;                        
  END SQLTOMETRICS;
  
  
    -- Trace from a ref cursor
  FUNCTION TRACE(p IN RCUR) RETURN INT IS
    metrics METRIC_ARR;
  BEGIN
    metrics := REFCURTOMETRICSINONLY(p);
    IF(metrics IS NOT NULL AND metrics.COUNT > 0) THEN
      FOR i in 1..metrics.COUNT LOOP
        LOGGING.tcplog(metrics(i).JSONMS());
      END LOOP;
      RETURN metrics.COUNT;
    END IF;
    RETURN 0;
  END TRACE;
  
  -- *******************************************************
  --    Get current XID function
  -- *******************************************************
  FUNCTION CURRENTXID RETURN RAW IS
    txid    VARCHAR2(50) := DBMS_TRANSACTION.local_transaction_id;
    idx     pls_integer;
    xid     RAW(8);
    xid_usn  NUMBER;
    xid_slot NUMBER;
    xid_sqn  NUMBER;
    pos1    NUMBER;
    pos2    NUMBER;
  BEGIN
    IF txid IS NULL THEN
        --  ALSO SEE dbms_transaction.step_id
      txid := DBMS_TRANSACTION.local_transaction_id(true);
    END IF;
    pos1 := instr(txid, '.', 1, 1);
    pos2 := instr(txid, '.', pos1+1, 1);
    xid_usn := TO_NUMBER(substr(txid,1,pos1-1));
    xid_slot := TO_NUMBER(substr(txid,pos1+1,pos2-pos1));
    xid_sqn := TO_NUMBER(substr(txid,pos2+1));
    --SNAPTX;
    SELECT XID INTO xid FROM V$TRANSACTION WHERE XIDUSN = xid_usn AND XIDSLOT = xid_slot AND XIDSQN = xid_sqn AND STATUS = 'ACTIVE';
    return xid;
  END CURRENTXID;
  

  BEGIN
    UTL_HTTP.SET_PERSISTENT_CONN_SUPPORT(TRUE, 3);    
END TSDB_TRACER;



/*
select * from v$sessmetric;
select * from v$session_event;
select * from v$session_wait;
select * from v$sesstat;
select * from v$sysmetric;
select * from v$system_event;
select * from v$buffer_pool_statistics;
select * from v$buffer_pool;
select * from v$datafile;
select * from v$db_object_cache;
select * from v$enqueue_stat;
select * from v$eventmetric;
select * from v$event_name;
select * from v$filemetric;
select * from v$filestat;
select * from v$latch;
select * from v$object_usage;
select * from v$open_cursor;
select * from v$pgastat;
select * from v$process;
select * from v$rollstat;
select * from v$sga;
select * from v$sgastat;
select * from v$sort_usage;
select * from v$sqlarea;
select * from v$sql_cursor;
select * from v$sysstat;
select * from v$waitstat;
*/

/*
select value(t) from table(tsdb_tracer.sqltometrics(
  q'#      SELECT M.VALUE, TSDB_UTIL.CLEAN(N.NAME) NAME, 'CLASS', TSDB_TRACER.DECODE_CLASS(N.CLASS) CLAZZ
      FROM v$mystat M, v$statname N
      WHERE M.STATISTIC# = N.STATISTIC#
      AND EXISTS (
        SELECT COLUMN_VALUE FROM TABLE(TSDB_UTIL.USERSTATKEYS())
        WHERE COLUMN_VALUE = TSDB_UTIL.CLEAN(N.NAME)
      )#')) t;
      
      
declare
  tags TAGPAIR_ARR := TAGPAIR_ARR(TAGPAIR('aaa', 'zzz'), TAGPAIR('bbb', 'yyy'), TAGPAIR('ccc', 'xxx'));
  cnt INT := -1;
begin
  SELECT COUNT(*) INTO cnt FROM TABLE(tags) T WHERE K = 'aaa';
  DBMS_OUTPUT.PUT_LINE('COUNT of aaa:' || cnt);
  SELECT R INTO cnt FROM (SELECT ROWNUM R, T.K KEY FROM TABLE(tags) T) V WHERE V.KEY = 'bbb';
  DBMS_OUTPUT.PUT_LINE('OFFSET of bbb:' || cnt);
  INSERT INTO TABLE(tags) VALUES('eee', 'www');
end;


--select * from table(tsdb_tracer.sqltometrics(
--  q'#      SELECT M.VALUE, TSDB_UTIL.CLEAN(N.NAME) NAME, 'CLASS', TSDB_TRACER.DECODE_CLASS(N.CLASS) CLAZZ, 'FOO', 'BAR', SYSDATE - 1000
--      FROM v$mystat M, v$statname N
--      WHERE M.STATISTIC# = N.STATISTIC#
--      AND EXISTS (
--        SELECT COLUMN_VALUE FROM TABLE(TSDB_UTIL.USERSTATKEYS())
--        WHERE COLUMN_VALUE = TSDB_UTIL.CLEAN(N.NAME)
--      )#')) t;

begin TSDB_TRACER.CLEARTIMERS; END;

select TSDB_TRACER.STARTTIMERMETRIC(METRIC.PARSEMETRIC('sys.cpu:xhost=BBB'))
  .SLEEP(5)
  .CLOSE().JSONMS()
  from dual;
  
select TSDB_TRACER.STOPTIMER('sys.cpu:host=aaa,host=rv-wk-dmon-03,app=orcl,user=pltsdb').JSONMS() from dual;

-- 22-DEC-16 04.51.24.491343 PM], st: [22-DEC-16 04.51.19.476673000 PM]
select to_timestamp('22-DEC-16 04.51.24.491343 PM', 'DD-MM-RR HH.MI.SS.FF AM') from dual

select to_timestamp('22-DEC-16 04.51.24.491343 PM', 'DD-MM-RR HH.MI.SS.FF AM')  - to_timestamp('22-DEC-16 04.51.19.476673000 PM', 'DD-MM-RR HH.MI.SS.FF AM')
from dual

declare
  fromtime TIMESTAMP := to_timestamp('22-DEC-16 04.51.19.476673000 PM', 'DD-MM-RR HH.MI.SS.FF AM');
  totime TIMESTAMP := to_timestamp('22-DEC-16 04.51.24.491343 PM', 'DD-MM-RR HH.MI.SS.FF AM');
  delta CONSTANT INTERVAL DAY (9) TO SECOND  := totime - fromtime;
  elapsed NUMBER;
begin
  DBMS_OUTPUT.PUT_LINE('INTERVAL:' || delta);
  elapsed := TSDB_UTIL.ELAPSEDMS(fromtime, totime);
--  elapsed := ROUND(
--      (
--      (extract(day from delta)*24*60*60) + 
--      (extract(hour from delta)*60*60) + 
--      (extract(minute from delta)*60) + 
--      extract(second from delta)
--      ) * 1000
--    ,0);  
  DBMS_OUTPUT.PUT_LINE('ELAPSED:' || elapsed);
end;




      
*/

/
