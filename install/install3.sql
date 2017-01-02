--------------------------------------------------------
--  DDL for Type TAGPAIR
--------------------------------------------------------

  CREATE OR REPLACE TYPE TAGPAIR force as object ( 
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
CREATE OR REPLACE TYPE BODY TAGPAIR as

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
END;

/

--------------------------------------------------------
--  DDL for Type TAGPAIR_ARR
--------------------------------------------------------

  CREATE OR REPLACE TYPE TAGPAIR_ARR IS  TABLE OF TAGPAIR;

/

--------------------------------------------------------
--  DDL for Type METRIC
--------------------------------------------------------

  CREATE OR REPLACE TYPE METRIC FORCE AS OBJECT (
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
  -- Same as CLOSE except also unregisters itself from the TSDB_TRACER timers map
  MEMBER FUNCTION CLOSETIMER(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP, mvalue IN NUMBER DEFAULT NULL) RETURN METRIC,  
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
CREATE OR REPLACE TYPE BODY METRIC AS

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
    LOGGING.debug('METRIC OPEN: st: [' || me.START_TIME || ']' );
    RETURN me;
  END OPEN;
  
  -- Completes a timing computing the elapsed time from the passed timestamp (default SYSTIMESTAMP)
  -- If the passed value is null, the metric value is the elapsed time in ms.
  MEMBER FUNCTION CLOSE(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP, mvalue IN NUMBER DEFAULT NULL) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    IF(mvalue IS NULL) THEN
      me.VALUE := TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts);
      LOGGING.debug('METRIC CLOSE: value set: [' || me.VALUE || '], calced elapsed: [' || TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts) || '], ts: [' || ts || '], st: [' || me.START_TIME || ']' );
    ELSE
      me.VALUE := mvalue;
      me.TIMING := TSDB_UTIL.ELAPSEDMS(me.START_TIME, ts);
    END IF;    
    RETURN me;
  END CLOSE;

  -- Same as CLOSE except also unregisters itself from the TSDB_TRACER timers map
  MEMBER FUNCTION CLOSETIMER(ts IN TIMESTAMP DEFAULT SYSTIMESTAMP, mvalue IN NUMBER DEFAULT NULL) RETURN METRIC AS
    me METRIC := SELF;
  BEGIN
    me := me.CLOSE(ts, mvalue);
    EXECUTE IMMEDIATE 'BEGIN TSDB_TRACER.CLEARTIMER(:key); END;' USING me.METRICKEY();
    --TSDB_TRACER.CLEARTIMER(me.METRICKEY());
    RETURN me;
  END CLOSETIMER;
  
  
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
    resolvedTs NUMBER;
  BEGIN
    met_ric.TAGS := METRIC.SORT(met_ric.TAGS);
    -- If metric's timestamp is null, we have to set it
      -- Use incoming ts if not null, otherwise
      -- Use TSDB_UTIL.CURRENTMS();
    -- If metric's timestamp is NOT null *AND* an incoming ts is NOT null
      -- Set the metric's timestamp to ts
    IF(met_ric.tstamp IS NULL) THEN
      met_ric.TSTAMP := NVL(ts, TSDB_UTIL.CURRENTMS());
    ELSE
      IF(ts IS NOT NULL) THEN
        met_ric.TSTAMP := ts;
      END IF;
    END IF;  
    IF(tsInMs) THEN
      resolvedTs := NVL(ts, met_ric.TSTAMP);
    ELSE
      resolvedTs := NVL(ts, met_ric.TSTAMP)/1000;
    END IF;
    IF(asJson) THEN
      met := '{"metric":"' || met_ric.METRICNAME || '","value":' ||  TSDB_UTIL.NTOV(met_ric.VALUE) || ',"timestamp":' || resolvedTs || ',"tags":{';
      delim := ',';
      endchar := '}}';
    ELSE
      -- put $metric $now $value dc=$DC host=$HOST
      met := 'put ' || met_ric.METRICNAME || ' ' || resolvedTs || ' ' ||  TSDB_UTIL.NTOV(met_ric.VALUE) || ' ';
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

  CREATE OR REPLACE TYPE METRIC_ARR as table of metric;

/
