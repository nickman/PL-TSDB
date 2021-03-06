--------------------------------------------------------
--  DDL for Package TSDB_TRACER
--------------------------------------------------------
create or replace PACKAGE TSDB_TRACER authid definer AS 
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

  --==================================================================================
  -- DBMS_APPLICATION_INFO Metric Tag Keys
  --==================================================================================
  /** The DBMS_APPLICATION_INFO.CLIENT_INFO Metric Tag Key */
  DAI_CLIENT_TAG_KEY CONSTANT VARCHAR2(5) := 'cinfo';
  /** The DBMS_APPLICATION_INFO.MODULE Metric Tag Key */
  DAI_MODULE_TAG_KEY CONSTANT VARCHAR2(6) := 'module';
  /** The DBMS_APPLICATION_INFO.ACTION Metric Tag Key */
  DAI_ACTION_TAG_KEY CONSTANT VARCHAR2(6) := 'action';

  --==================================================================================
  -- Tag and metric formating token constants
  --==================================================================================
  /** The replacement token for the name of the value column */
  METRIC_TOKEN_VAL_COLUMN CONSTANT VARCHAR2(6) := '#VCOL#';

  TYPE METRIC_REC IS RECORD (
    METRIC_ID NUMBER,
    XROWID VARCHAR2(18),
    M METRIC
  );
  TYPE QCUR IS REF CURSOR RETURN METRIC_REC;
  
  
  -- ===================================================
  --   NEW NEW NEW
  -- ===================================================
  FUNCTION METRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR;
  PROCEDURE METRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR);
  
  FUNCTION CLOSEMETRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR;
  PROCEDURE CLOSEMETRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR);
  
  PROCEDURE CLEARMETRICSETS;
  PROCEDURE CLEARMETRICSETS(setName IN VARCHAR2);
  
  FUNCTION METRICSETS RETURN NAMEVALUE_ARR;
  
  
  
  -- ===================================================
  
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
  
  --===================================================================================================================
  --  Clears the named timer
  --===================================================================================================================
  PROCEDURE CLEARTIMER(key IN VARCHAR2);
  


  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICS(p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR;  
  
  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics.
  -- Only works, so far as I know, in Oracle 12+
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICSINONLY(p IN SYS_REFCURSOR) RETURN METRIC_ARR;
  
  --====================================================================================================
  -- Packs all the passed metric arrays into one metric array
  --====================================================================================================
  FUNCTION PACK(metricsArrArr IN METRIC_ARR_ARR) RETURN METRIC_ARR;
    
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
  -- Trace all metrics and return the the number of metrics traced
  FUNCTION TRACE(metrics IN METRIC_ARR) RETURN INT;  
  -- Trace all metrics and return a count
  FUNCTION TRACEF(metrics IN METRIC_ARR) RETURN METRIC_ARR;

  
  FUNCTION INDIRECT(p IN SYS_REFCURSOR) RETURN SYS_REFCURSOR;
  
  FUNCTION USERSTATSREF RETURN SYS_REFCURSOR;
  
  --====================================================================================================
  -- Returns a ref cursor of persisted metrics
  --====================================================================================================
  FUNCTION METRIC_REFCUR(locked IN INT DEFAULT 0, maxrows IN INT DEFAULT 1000) RETURN SYS_REFCURSOR;
  
  --====================================================================================================
  -- Pipes out a batch of persisted metrics
  --====================================================================================================
  FUNCTION PIPE_METRICS(p IN QCUR, maxrowCount IN INT DEFAULT 1000) RETURN METRIC_EXT_ARR PIPELINED PARALLEL_ENABLE(PARTITION p BY ANY);
  
  --====================================================================================================
  -- Persists an array of METRICs to the persistent queue
  --====================================================================================================
  PROCEDURE SAVE_METRICS(metrics IN METRIC_ARR);
  
  /**
   * Adds DBMS_APPLICATION_INFO tags to the passed metric if the tag values are not null
   * @param m The metric to add the tags to
   * @param dotrepl An optional character to replace '.' in the tag values. Defaults to ''   
   * @return the tagged metric
   */
  FUNCTION APPINFOTAGS(m IN METRIC, dotrepl IN CHAR DEFAULT '') RETURN METRIC;
  
  /**
   * Adds DBMS_APPLICATION_INFO tags to the passed metrics if the tag values are not null
   * @param m The metrics to add the tags to
   * @param dotrepl An optional character to replace '.' in the tag values. Defaults to ''
   * @return the tagged metrics
   */
  FUNCTION APPINFOTAGS(m IN METRIC_ARR, dotrepl IN CHAR DEFAULT '') RETURN METRIC_ARR;
  
  
  -- *******************************************************
  --    Get current XID function
  -- *******************************************************
  FUNCTION CURRENTXID RETURN RAW;  

END TSDB_TRACER;

/


--------------------------------------------------------
--  DDL for Package Body TSDB_TRACER
--------------------------------------------------------

create or replace PACKAGE BODY TSDB_TRACER AS

  /* The metric stack */
  metricStack XMETRIC_ARR_STACK;
  /* The current metric stack depth */
  depth PLS_INTEGER := 0;  
  /* The named metrics map */
  namedMetrics XMETRIC_NAMED_ARR;
  /* A map of timer metrics keyed by the metric key */
  timers XMETRIC_ARR;
  
  -- ===================================================
  --   NEW NEW NEW
  -- ===================================================
  FUNCTION METRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR IS
  BEGIN
    NULL;
  END METRICSET;
  
  PROCEDURE METRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) IS
  BEGIN
    NULL;
  END METRICSET;
  
  FUNCTION CLOSEMETRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR IS
  BEGIN
    NULL;
  END CLOSEMETRICSET;
  
  
  PROCEDURE CLOSEMETRICSET(setName IN VARCHAR2, p IN OUT SYS_REFCURSOR) IS
  BEGIN
    NULL;
  END CLOSEMETRICSET;
  
  PROCEDURE CLEARMETRICSETS IS
  BEGIN
    NULL;
  END CLEARMETRICSETS;
  
  PROCEDURE CLEARMETRICSETS(setName IN VARCHAR2) IS
  BEGIN
    NULL;
  END CLEARMETRICSETS;
  
  FUNCTION METRICSETS RETURN NAMEVALUE_ARR IS
  BEGIN
    NULL;
  END METRICSETS;
  
  
  
  -- ===================================================
  
  
  --====================================================================================================
  -- Returns a ref cursor of persisted metrics
  --====================================================================================================
  FUNCTION METRIC_REFCUR(locked IN INT DEFAULT 0, maxrows IN INT DEFAULT 1000) RETURN SYS_REFCURSOR IS
    pout SYS_REFCURSOR;
  BEGIN
    IF(locked=0) THEN
      OPEN pout FOR  SELECT T.METRIC_ID, ROWIDTOCHAR(T.ROWID), T.M FROM TSDB_METRIC_QUEUE T ORDER BY METRIC_ID;
    ELSE
      OPEN pout FOR  SELECT T.METRIC_ID, ROWIDTOCHAR(T.ROWID), T.M FROM TSDB_METRIC_QUEUE T FOR UPDATE SKIP LOCKED ORDER BY METRIC_ID;
    END IF;
    RETURN pout;
  END METRIC_REFCUR;
  
  
  --====================================================================================================
  -- Pipes out a batch of persisted metrics
  --====================================================================================================
  FUNCTION PIPE_METRICS(p IN QCUR, maxrowCount IN INT DEFAULT 1000) RETURN METRIC_EXT_ARR PIPELINED PARALLEL_ENABLE(PARTITION p BY ANY) IS
    mkey VARCHAR2(1000);
    done PLS_INTEGER := 0;
    maxrows CONSTANT PLS_INTEGER := maxrowCount;
    mid NUMBER;
    m METRIC;
    xrow VARCHAR2(18);
  BEGIN    
    LOOP
      FETCH p INTO mid, xrow, m;
      EXIT WHEN p%NOTFOUND;
      --LOGGING.tcplog('PIPE_METRICS: Fetched:' || met.METRIC_STRING);
      PIPE ROW (METRIC_EXT(mid, xrow, m));
      done := done +1;
      IF(done = maxrows) THEN EXIT; END IF;
    END LOOP;
    LOGGING.debug('PIPE_METRICS: Fetched Rows:' || done);
    CLOSE p;
    RETURN;
    EXCEPTION 
      WHEN NO_DATA_NEEDED THEN
        LOGGING.debug('PIPE_METRICS: Fetched Rows:' || done);
        RETURN;
      WHEN OTHERS THEN
        IF(p%ISOPEN) THEN
          CLOSE p;
        END IF;    
        DECLARE
          errm VARCHAR2(200) := SQLERRM();
        BEGIN
          LOGGING.error('PIPE_METRICS ERROR: errm:' || errm || ', backtrace:' || dbms_utility.format_error_backtrace);
          RAISE;                    
        END;
  END PIPE_METRICS;
  
  FUNCTION NEXTSEQ RETURN NUMBER IS
    s NUMBER;
  BEGIN
    SELECT TSDB_METRIC_SEQ.NEXTVAL INTO s FROM DUAL;
    RETURN s;
  END NEXTSEQ;
  
  --====================================================================================================
  -- Persists an array of METRICs to the persistent queue
  --====================================================================================================
  PROCEDURE SAVE_METRICS(metrics IN METRIC_ARR) IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    ts CONSTANT NUMBER := TSDB_UTIL.CURRENTMS;
  BEGIN
    FORALL i IN 1..metrics.COUNT 
      INSERT INTO TSDB_METRIC_QUEUE (METRIC_ID, M) VALUES (TSDB_METRIC_SEQ.NEXTVAL , metrics(i).TS(ts));
    COMMIT;
  END SAVE_METRICS;
  
  
  
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
  --  Clears the named timer
  --===================================================================================================================
  PROCEDURE CLEARTIMER(key IN VARCHAR2) IS 
  BEGIN
    IF(timers.EXISTS(key)) THEN
      timers.DELETE(key);
    END IF;
  END CLEARTIMER;
  
  
  

  --===================================================================================================================
  --  Returns the user stats as metrics
  --===================================================================================================================
  FUNCTION USERSTATS RETURN METRIC_ARR IS
    p SYS_REFCURSOR;
    sqlText VARCHAR2(1000) := 'SELECT M.VALUE, TSDB_UTIL.CLEAN(N.NAME) NAME, ''CLASS'', TSDB_TRACER.DECODE_CLASS(N.CLASS) CLAZZ  FROM v$mystat M, v$statname N  WHERE M.STATISTIC# = N.STATISTIC# AND EXISTS (SELECT COLUMN_VALUE FROM TABLE(TSDB_UTIL.USERSTATKEYS())  WHERE COLUMN_VALUE = TSDB_UTIL.CLEAN(N.NAME))';
  BEGIN
--    LOGGING.tcplog(sqlText);
--    RETURN SQLTOMETRICS(sqlText);
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
  
  
  FUNCTION USERSTATSREF RETURN SYS_REFCURSOR IS
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
    RETURN p;  
  END USERSTATSREF;
  


    -- Decodes a class number to the class name
  FUNCTION DECODE_CLASS(classNum IN PLS_INTEGER) RETURN VARCHAR2 IS
    name VARCHAR2(40);
  BEGIN
    select 
    decode (bitand(  1,classNum),  1,'User',              '') ||
    decode (bitand(  2,classNum),  2,'Redo',              '') ||
    decode (bitand(  4,classNum),  4,'Enqueue',           '') ||
    decode (bitand(  8,classNum),  8,'Cache',             '') ||
    decode (bitand( 16,classNum), 16,'Parallel Server',   '') ||
    decode (bitand( 32,classNum), 32,'OS',                '') ||
    decode (bitand( 64,classNum), 64,'SQL',               '') ||
    decode (bitand(128,classNum),128,'Debug',             '') INTO name FROM DUAL;
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
--    arr XMETRIC_ARR;
    m METRIC;
  BEGIN
--    arr := metricStack(mdepth);
      key := metricStack(mdepth).first;
      WHILE (key IS NOT NULL) LOOP
        IF(LOGGING.IS_DEBUG_ENABLED) THEN
          LOGGING.debug('POST:' || metricStack(mdepth)(key).METRICNAME || ':' || metricStack(mdepth)(key).VALUE || '......[' || metricStack(mdepth)(key).TSTAMP || ']');
        END IF;
        key := metricStack(mdepth).next(key);
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
  
  -- Trace all metrics and return the the number of metrics traced
  FUNCTION TRACE(metrics IN METRIC_ARR) RETURN INT IS    
  BEGIN
    TRACE(metrics);
    RETURN metrics.COUNT;
  END TRACE;
  

  -- Trace all metrics and return the array
  FUNCTION TRACEF(metrics IN METRIC_ARR) RETURN METRIC_ARR IS
  BEGIN
    TRACE(metrics);
    RETURN metrics;
  END TRACEF;
  
  --====================================================================================================
  -- Packs all the passed metric arrays into one metric array
  --====================================================================================================
  FUNCTION PACK(metricsArrArr IN METRIC_ARR_ARR) RETURN METRIC_ARR IS
    packed METRIC_ARR := METRIC_ARR();
    idx PLS_INTEGER := 1;
  BEGIN
    LOGGING.log('PACKing [' || metricsArrArr.COUNT || '] METRIC Arrays');
    FOR i IN 1..metricsArrArr.COUNT LOOP
      FOR x IN 1..metricsArrArr(i).COUNT LOOP
        packed.extend();
        IF(metricsArrArr(i)(x) IS NOT NULL) THEN
          packed(idx) := metricsArrArr(i)(x);
          idx := idx + 1;
        END IF;
      END LOOP;
      --SELECT VALUE(T) BULK COLLECT INTO packed FROM TABLE(metricsArrArr(i)) T;
    END LOOP;
    LOGGING.log('PACKed [' || packed.COUNT || '] METRICs');
    RETURN packed;
  END PACK;
  
  /**
   * Adds DBMS_APPLICATION_INFO tags to the passed metric if the tag values are not null
   * @param m The metric to add the tags to
   * @param dotrepl An optional character to replace '.' in the tag values. Defaults to ''   
   * @return the tagged metric
   */
  FUNCTION APPINFOTAGS(m IN METRIC, dotrepl IN CHAR DEFAULT '') RETURN METRIC IS
    c VARCHAR2(64);
    module VARCHAR2(48);
    action VARCHAR2(32);
    mx METRIC := m;
  BEGIN
    IF(module IS NOT NULL) THEN module := REPLACE(TSDB_UTIL.CLEAN(module), '.', dotrepl); END IF;
    IF(action IS NOT NULL) THEN action := REPLACE(TSDB_UTIL.CLEAN(action), '.', dotrepl); END IF;  
    DBMS_APPLICATION_INFO.READ_CLIENT_INFO(c);
    DBMS_APPLICATION_INFO.READ_MODULE(module, action);
    IF(c IS NOT NULL) THEN
      mx := mx.pushTag(DAI_CLIENT_TAG_KEY,c);
    END IF;
    IF(module IS NOT NULL) THEN
      mx := mx.pushTag(DAI_MODULE_TAG_KEY,module);
    END IF;
    IF(action IS NOT NULL) THEN
      mx := mx.pushTag(DAI_ACTION_TAG_KEY,action);
    END IF;    
    RETURN mx;
  END APPINFOTAGS;
  
  /**
   * Adds DBMS_APPLICATION_INFO tags to the passed metrics if the tag values are not null
   * @param m The metrics to add the tags to
   * @param dotrepl An optional character to replace '.' in the tag values. Defaults to ''   
   * @return the tagged metrics
   */
  FUNCTION APPINFOTAGS(m IN METRIC_ARR, dotrepl IN CHAR DEFAULT '') RETURN METRIC_ARR IS
    c VARCHAR2(64);
    module VARCHAR2(48);
    action VARCHAR2(32);
    mx METRIC_ARR := METRIC_ARR();
  BEGIN
    DBMS_APPLICATION_INFO.READ_CLIENT_INFO(c);
    DBMS_APPLICATION_INFO.READ_MODULE(module, action);
    IF(c IS NOT NULL) THEN c := TSDB_UTIL.CLEAN(c); END IF;
    IF(module IS NOT NULL) THEN module := REPLACE(TSDB_UTIL.CLEAN(module), '.', dotrepl); END IF;
    IF(action IS NOT NULL) THEN action := REPLACE(TSDB_UTIL.CLEAN(action), '.', dotrepl); END IF;
    mx.extend(m.COUNT);
    FOR i in 1..m.COUNT LOOP
      IF(c IS NOT NULL) THEN
        mx(i) := m(i).pushTag(DAI_CLIENT_TAG_KEY,c);
      END IF;
      IF(module IS NOT NULL) THEN
        mx(i) := m(i).pushTag(DAI_MODULE_TAG_KEY,module);
      END IF;
      IF(action IS NOT NULL) THEN
        mx(i) := m(i).pushTag(DAI_ACTION_TAG_KEY,action);
      END IF;        
    END LOOP;
    RETURN mx;
  END APPINFOTAGS;
  
  
  -- Posts the passed content to the configured HTTP endpoint
  PROCEDURE POST(content IN CLOB) IS
    req   UTL_HTTP.REQ;
    resp  UTL_HTTP.RESP;    
    postUrl VARCHAR2(1000) := 'http://' || TSDB_UTIL.CFG_TRACING_HTTP_HOST || ':' || TSDB_UTIL.CFG_TRACING_HTTP_PORT || TSDB_UTIL.CFG_TRACING_HTTP_URI;
  BEGIN
      req := UTL_HTTP.BEGIN_REQUEST (url=> postUrl, method => 'POST');
      UTL_HTTP.SET_HEADER (r =>  req, name   =>  'Content-Type', value  =>  'application/json;charset=UTF-8');
      UTL_HTTP.SET_HEADER (r =>  req, name => 'Content-Length', value => length(content));
      UTL_HTTP.WRITE_TEXT (r =>  req, data => content);    
      resp := UTL_HTTP.GET_RESPONSE(req);
      UTL_HTTP.END_RESPONSE(resp);          
      EXCEPTION WHEN OTHERS THEN
        DECLARE
          errm VARCHAR2(200) := SQLERRM();
        BEGIN
          LOGGING.error('POST ERROR: errm:' || errm);
        END;
        RAISE;
  END POST;
  
  
  
  -- Trace all metrics
  -- Having persistent trouble with chunked transfers,
  -- so posts are sent in individual bodies
  -- of <= 32767 bytes
  PROCEDURE TRACE(metrics IN METRIC_ARR) IS
    content CLOB;
    jsonText VARCHAR2(400);
    now TIMESTAMP(9) := SYSTIMESTAMP;
    metricCount CONSTANT INT := metrics.COUNT;
    startTime CONSTANT NUMBER := DBMS_UTILITY.GET_TIME;
    buffer VARCHAR2(2000) := NULL;
    contentLength PLS_INTEGER := 0;
    jsonLength PLS_INTEGER := 0;
    amount PLS_INTEGER := 2000;
    offset PLS_INTEGER := 1;
    chunked BOOLEAN := false;
    maxSize CONSTANT PLS_INTEGER := 32765;    
    metricsDone BOOLEAN := FALSE;
    totalPayloadSize PLS_INTEGER := 1;
  BEGIN
    --LOGGING.log('TRACING [' || metrics.COUNT || '] metrics');
    DBMS_LOB.CREATETEMPORARY(content, true, DBMS_LOB.CALL);
    DBMS_LOB.OPEN(content, DBMS_LOB.LOB_READWRITE);
    DBMS_LOB.WRITEAPPEND(content, 1, '[');      
    FOR i IN 1..metrics.COUNT LOOP      
      jsonText := metrics(i).JSONMS();
      jsonLength := length(jsonText);      
      --LOGGING.log('Metric #' || i || ':' || jsonLength);
      IF((contentLength + jsonLength) > maxSize) THEN
        DBMS_LOB.WRITEAPPEND(content, 1, ']');   
        totalPayloadSize := totalPayloadSize + 1;
        --LOGGING.log('Posting [' || DBMS_LOB.GETLENGTH(content) || '] bytes');
        POST(content);
        --LOGGING.log('Reseting Content Buffer [' || DBMS_LOB.GETLENGTH(content) || '] from 2 to [' || contentLength || ']');
        DBMS_LOB.TRIM(content, 1); -- Delete everything except the json array opener
        --LOGGING.log('Reset Content Buffer <' || TO_CHAR(content) || '>, Total So Far: [' || totalPayloadSize || ']');
        contentLength := 1;
        metricsDone := TRUE;
      END IF;      
      IF(DBMS_LOB.GETLENGTH(content) > 2) THEN
        DBMS_LOB.WRITEAPPEND(content, 1, ',');      
        contentLength := contentLength + 1;
        totalPayloadSize := totalPayloadSize + 1;
        --LOGGING.log('Appended Com');
      END IF;      
      DBMS_LOB.WRITEAPPEND(content, jsonLength, jsonText);      
      --LOGGING.log('Appended [' || jsonLength || '] for total [' || DBMS_LOB.GETLENGTH(content) || '] bytes');
      contentLength := contentLength + jsonLength;      
      totalPayloadSize := totalPayloadSize + jsonLength;
      metricsDone := FALSE;
    END LOOP;
    IF(metricsDone != TRUE) THEN
      --LOGGING.log('Metrics Not Done');
        DBMS_LOB.WRITEAPPEND(content, 1, ']');   
        --LOGGING.log('Final Posting [' || DBMS_LOB.GETLENGTH(content) || '] bytes');
        POST(content);    
        totalPayloadSize := totalPayloadSize + 1;
    END IF;
    DBMS_LOB.CLOSE(content);
    DBMS_LOB.FREETEMPORARY(content);    
    LOGGING.log('Traced [' || metricCount || '] metrics in [' || ((DBMS_UTILITY.GET_TIME - startTime)*10) || '] ms. Payload Size: [' || totalPayloadSize || '] bytes');
    EXCEPTION WHEN OTHERS THEN 
        BEGIN
          DBMS_LOB.CLOSE(content);
          EXCEPTION WHEN OTHERS THEN NULL;
        END;
        BEGIN
          DBMS_LOB.FREETEMPORARY(content);
          EXCEPTION WHEN OTHERS THEN NULL;
        END;
        DECLARE
          errm VARCHAR2(200) := SQLERRM();
        BEGIN
          LOGGING.error('TRACE ERROR: errm:' || errm);
        END;
        RAISE;                    
  END TRACE;
  
    -- Closes any persistent connections
  PROCEDURE CLOSE_PERSISTENT_CONNS IS
  BEGIN
    UTL_HTTP.CLOSE_PERSISTENT_CONNS(host => TSDB_UTIL.CFG_TRACING_HTTP_HOST, port => TSDB_UTIL.CFG_TRACING_HTTP_PORT);
  END CLOSE_PERSISTENT_CONNS;

  

  FUNCTION INDIRECT(p IN SYS_REFCURSOR) RETURN SYS_REFCURSOR IS
    c SYS_REFCURSOR;
    x SYS_REFCURSOR;
    a VARCHAR2(2000);
  BEGIN    

    SELECT p INTO x FROM DUAL;
    CLOSE p;
--    SELECT DUMP(x) INTO A FROM DUAL;
--    LOGGING.tcplog('X CUR:' || A);
    RETURN x;
  END INDIRECT;
  
  FUNCTION BOOL_TO_CHAR(bool IN BOOLEAN) RETURN CHAR IS
  BEGIN
    IF(bool) THEN
      RETURN 'Y';
    END IF;
    RETURN 'N';
  END BOOL_TO_CHAR;

  PROCEDURE LOGDESCTAB(col IN PLS_INTEGER, d IN DBMS_SQL.DESC_TAB3) IS
    nullable CHAR(1);
  BEGIN
    IF(d(col).col_null_ok) THEN 
      nullable := 'Y';
    ELSE
      nullable := 'N';
    END IF;
    LOGGING.debug('COL #' || col || ' - ' || 'coltype:[' || d(col).col_type || '],' || 'colmaxlen:[' || d(col).col_max_len || '],' || 'colname:[' || d(col).col_name || '],' || 'colnamelen:[' || d(col).col_name_len || '],' || 'colschemaname:[' || d(col).col_schema_name || '],' || 'colschemanamelen:[' || d(col).col_schema_name_len || '],' || 'colprecision:[' || d(col).col_precision || '],' || 'colscale:[' || d(col).col_scale || '],' || 'colcharsetid:[' || d(col).col_charsetid || '],' || 'colcharsetform:[' || d(col).col_charsetform || '],' || 'colnullok:[' || nullable || '],' || 'coltypename:[' || d(col).col_type_name || '],' || 'coltypenamelen:[' || d(col).col_type_name_len || ']');
  END LOGDESCTAB;
  
  
  FUNCTION IS_CHAR_TYPE(colType IN PLS_INTEGER) RETURN BOOLEAN IS
  BEGIN
    RETURN (
      colType = DBMS_TYPES.TYPECODE_VARCHAR2 
      OR colType = DBMS_TYPES.TYPECODE_NVARCHAR2 
      OR colType = DBMS_TYPES.TYPECODE_VARCHAR 
      OR colType = DBMS_TYPES.TYPECODE_CHAR 
      OR colType = DBMS_TYPES.TYPECODE_NCHAR);    
  END IS_CHAR_TYPE;
  
  FUNCTION IS_NUM_OR_DATE(colType IN PLS_INTEGER) RETURN BOOLEAN IS
  BEGIN
    RETURN (
      colType = DBMS_TYPES.TYPECODE_TIMESTAMP
      OR colType = DBMS_TYPES.TYPECODE_TIMESTAMP_TZ
      OR colType = DBMS_TYPES.TYPECODE_TIMESTAMP_LTZ
      OR colType = DBMS_TYPES.TYPECODE_DATE
      OR colType = DBMS_TYPES.TYPECODE_NUMBER
    );
  END IS_NUM_OR_DATE;
  
  -- Extracts the timestamp from the current row in ms.
  FUNCTION GET_ROW_TS(cursorNum IN OUT NOCOPY NUMBER, tsType IN PLS_INTEGER, colcnt IN NUMBER) RETURN NUMBER IS
    tsTimestamp TIMESTAMP;
    tsTimestampWTimeZone TIMESTAMP WITH TIME ZONE;
    tsDate DATE;
    tsNumber NUMBER;
  BEGIN
      IF(tsType=DBMS_TYPES.TYPECODE_TIMESTAMP) THEN
        DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestamp);
        RETURN TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestamp);
      ELSEIF(tsType=DBMS_TYPES.TYPECODE_DATE) THEN       
        DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsDate);
        RETURN TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, TSDB_UTIL.DATETOTIMESTAMP(tsDate));
      ELSEIF(tsType=DBMS_TYPES.TYPECODE_NUMBER) THEN       
        DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsNumber);
        RETURN tsNumber;
      ELSE
        DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestampWTimeZone);
        RETURN TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestampWTimeZone);
      END IF;
  END GET_ROW_TS;

  
  --====================================================================================================
  -- Converts the results from the passed opened and parsed cursor number to an array of metrics.
  -- The cursor is closed on completion.
  -- Doc needed
  -- SPEC:  [TIMESTAMP # OR DATE OR TIMESTAMP],METRIC_NAME_FORMAT, [TAGKEY1FMT, TAGVALUE1FMT,TAGKEY1nFMT,TAGVALUEnFMT], VALUE1, [...VALUEn]
  --====================================================================================================
  FUNCTION CURSORINTTOMETRICS(cursorNum IN OUT NOCOPY NUMBER) RETURN METRIC_ARR IS
    --
    -- Move to package
    --
    --TYPE TAGMAP IS TABLE OF VARCHAR2(360) INDEX BY VARCHAR2(360);
    --

    --tags TAGMAP := TAGMAP();
    tags TAGPAIR_ARR := TAGPAIR_ARR();
    cntr PLS_INTEGER := 0;
    rows_processed INTEGER;    
    desctab  DBMS_SQL.DESC_TAB3;
    colcnt   NUMBER;   
    colnum INT;
    hasTs BOOLEAN;
    tsType PLS_INTEGER := NULL;
    tsTypeName VARCHAR2(200);
    tId PLS_INTEGER := 3;
    vId PLS_INTEGER := 1;
    mId PLS_INTEGER := 1;
    tagCount PLS_INTEGER;
    met METRIC := NULL;
    metrics METRIC_ARR := METRIC_ARR();

    metName VARCHAR2(100);
    metValue NUMBER;
    metValueColName VARCHAR2(100);
    tagK VARCHAR2(100);
    tagV VARCHAR2(100);
    rowsFetched PLS_INTEGER := 0;    
    tsTimestamp TIMESTAMP;
    tsTimestampWTimeZone TIMESTAMP WITH TIME ZONE;
    tsDate DATE;
    tsNumber NUMBER;
    typeCodes INT_ARR;
    metricNameFormat VARCHAR2(360);
    valuesIndex PLS_INTEGER := -1;
    tagsIndex PLS_INTEGER := -1;
    columnNames VARCHAR2_ARR := VARCHAR2_ARR();
    valueColumnNames VARCHAR2_ARR := VARCHAR2_ARR();
    tsType PLS_INTEGER := -1;    
    valuesCount PLS_INTEGER := 0;
    
  BEGIN
    DBMS_SQL.DESCRIBE_COLUMNS3(cursorNum, colcnt, desctab);    
    IF(colcnt < 2) THEN
      LOGDESCTAB(1, desctab);
      RETURN metrics;
    END IF;
    typeCodes.extend(colcnt);
    columnNames.extend(colcnt);
    FOR i IN 1..colcnt LOOP
      typeCodes(i) := desctab(i).col_type;
      columnNames(i) := desctab(i).col_name;
    END LOOP;
    
    
    -- Determine if the first column is NOT a String, meaning a timestamp is provided
    IF(IS_CHAR_TYPE(typeCodes(1))=FALSE) THEN
      -- If hasTs is true, then column(1) coltype must be a timestamp/date/number (i.e. a datatype from which a timestamp can be derived)
      IF(IS_NUM_OR_DATE(typeCodes(1)) = FALSE) THEN
        RAISE invalid_metrics_cursor;
      END IF;
      hasTs := TRUE;
      tsType := typeCodes(1);
      tagsIndex := 3;
      IF(tsType=DBMS_TYPES.TYPECODE_TIMESTAMP) THEN
        DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, tsTimestamp);
      ELSEIF(tsType=DBMS_TYPES.TYPECODE_DATE) THEN       
        DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, tsDate);
      ELSEIF(tsType=DBMS_TYPES.TYPECODE_NUMBER) THEN       
        DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, tsNumber);
      ELSE
        DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, tsTimestampWTimeZone);
      END IF;
      DBMS_SQL.DEFINE_COLUMN(cursorNum, 2, metricNameFormat, 100);      
    ELSE
      hasTs := FALSE;
      tagsIndex := 2;
      DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, metricNameFormat, 100);
    END IF;

    -- Find the index of the first +1 col that is a number (the index of the first value)
    FOR i IN 2..typeCodes.COUNT LOOP
      IF(typeCodes(i)=DBMS_TYPES.TYPECODE_NUMBER) THEN
        valuesIndex := i;
        EXIT;
      END IF;      
    END LOOP;
    -- If values index was not found, invalid cursor
    IF(valuesIndex=-1) THEN
      RAISE invalid_metrics_cursor;
    END IF;
    -- Value count is columnCount minus valuesIndex
    valuesCount := colcnt - valuesIndex;
    valueColumnNames.extend(valuesCount);
    -- Get tag count. If tag count is not zero or even, invalid cursor
    IF(valuesIndex = tagsIndex) THEN
      tagCount := 0;
    ELSE
      tagCount := valuesIndex - tagsIndex;
      IF(tagCount < 0 OR MOD(tagCount,2)!=0) THEN
        RAISE invalid_metrics_cursor;
      END IF;
    END IF;

--  /** The replacement token for the name of the value column */
--  METRIC_TOKEN_VAL_COLUMN CONSTANT VARCHAR2(6) := '#VCOL#';
    LOGGING.debug('CURSORINTTOMETRICS: Tag Pairs: ' || tagCount || ', TS: ' || BOOL_TO_CHAR(hasTs));
    tId := tagsIndex;    
    FOR i IN 1..tagCount LOOP
      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
      LOGGING.debug('Bound Tag Key [' || i || '] at position [' || tId || ']');
      tId := tId + 1;
      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
      --DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, tagV, 100);
      LOGGING.debug('Bound Tag Value [' || i || '] at position [' || tId || ']');
      tId := tId + 1;
    END LOOP;

    -- Index and define metric value columns
    LOGGING.debug('CURSORINTTOMETRICS: Tags Bound');
    FOR i IN valuesIndex..colcnt LOOP
      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, metValue);
      valueColumnNames(i-valuesCount+1) := columnNames(tId);
      tId := tId + 1;
    END LOOP;

    -- All columns now defined.
    LOGGING.debug('CURSORINTTOMETRICS: Executing...');
    
    LOOP      
      tId := 1;
      vId := 1;
      rowsFetched := DBMS_SQL.FETCH_ROWS(cursorNum);
      EXIT WHEN rowsFetched = 0;
      cntr := cntr + 1;        
      LOGGING.debug('Processing Row#' || cntr);
      IF(hasTs) THEN
        tsNumber := GET_ROW_TS(cursorNum, tsType, tId);
        tId := tId + 1;        
      END IF;
      DBMS_SQL.COLUMN_VALUE(cursorNum, tId, metricNameFormat);
      tId := tId + 1;
      -- Load Tags
      FOR i IN 1..tagCount LOOP
        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagK);
        tId := tId + 1;
        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagV);
        tId := tId + 1;
        tags(i) := TAGPAIR(TSDB_UTIL.CLEAN(tagK), TSDB_UTIL.CLEAN(tagV));
      END LOOP;
      -- Load Values
      FOR i IN 1..valuesCount LOOP
        -- columnNames(tId)
        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, metValue);
        metName := TSDB_UTIL.CLEAN(REPLACE(metricNameFormat, METRIC_TOKEN_VAL_COLUMN, columnNames(tId)));
        met := METRIC(metName, tags).VAL(metValue);
        IF(hasTs) THEN
          met := met.TS(tsNumber);
        END IF;
        metrics.extend();
        metrics(mId) := met; 
        vId := vId + 1;
        mId := mId + 1;
      END LOOP;


      tags.DELETE();
    END LOOP;


    tId := 3;
    colnum := desctab.first;
    LOGGING.debug('CURSORINTTOMETRICS: Executing...');
    --rowsFetched := DBMS_SQL.EXECUTE(cursorNum);
    LOGGING.debug('CURSORINTTOMETRICS: Executed: ' || rowsFetched);
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
    RETURN metrics;
    EXCEPTION WHEN OTHERS THEN       
      DECLARE
        errm VARCHAR2(200) := SQLERRM();
      BEGIN
        LOGGING.error('CURSORINTTOMETRICS ERROR: errm:' || errm);
        RAISE;                    
      END;
      
      --NULL;
      
  END CURSORINTTOMETRICS;

--  FUNCTION CURSORINTTOMETRICS(cursorNum IN OUT NOCOPY NUMBER) RETURN METRIC_ARR IS
--    cntr PLS_INTEGER := 0;
--    rows_processed INTEGER;    
--    desctab  DBMS_SQL.DESC_TAB3;
--    colcnt   NUMBER;   
--    colnum INT;
--    hasTs BOOLEAN;
--    tsType PLS_INTEGER := NULL;
--    tsTypeName VARCHAR2(200);
--    tId PLS_INTEGER := 3;
--    tagCount PLS_INTEGER;
--    met METRIC := NULL;
--    metrics METRIC_ARR := METRIC_ARR();
--    metName VARCHAR2(100);
--    metValue NUMBER;
--    tagK VARCHAR2(100);
--    tagV VARCHAR2(100);
--    rowsFetched PLS_INTEGER := 0;    
--    tsTimestamp TIMESTAMP;
--    tsTimestampWTimeZone TIMESTAMP WITH TIME ZONE;
--    tsDate DATE;
--    tsNumber NUMBER;
--  BEGIN
--    DBMS_SQL.DESCRIBE_COLUMNS3(cursorNum, colcnt, desctab);
--    IF(colcnt < 2) THEN
--      LOGDESCTAB(desctab(1));
--      RETURN metrics;
--    END IF;
--    hasTs := MOD(colcnt, 2) != 0;
--    IF(hasTs) THEN
--      tagCount := (colcnt - 3) / 2;
--    ELSE
--      tagCount := (colcnt - 2) / 2;
--    END IF;
--    DBMS_SQL.DEFINE_COLUMN(cursorNum, 1, metValue); 
--    DBMS_SQL.DEFINE_COLUMN(cursorNum, 2, metName, 100); 
--    IF(hasTs) THEN
--      tsType := desctab(colcnt).col_type;
--      tsTypeName := desctab(colcnt).col_type_name;
--      LOGGING.debug('TSTYPE: [' || tsType || '], TS NAME: [' || tsTypeName || '], COLCNT: [' || colcnt || ']');
--      CASE tsType
--        WHEN DBMS_TYPES.TYPECODE_TIMESTAMP THEN
--          LOGGING.debug('DEFINE TIMESTAMP');
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt,  tsTimestamp);
--        WHEN 181 THEN
--          LOGGING.debug('DEFINE TIMESTAMP WITH TZ');
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsTimestampWTimeZone);          
--        WHEN DBMS_TYPES.TYPECODE_DATE THEN
--          LOGGING.debug('DEFINE DATE');
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsDate);
--        WHEN DBMS_TYPES.TYPECODE_NUMBER THEN
--          LOGGING.debug('DEFINE NUMBER');
--          DBMS_SQL.DEFINE_COLUMN(cursorNum, colcnt, tsNumber);
--        ELSE
--          RAISE unsupported_timestamp_type;
--      END CASE;
--    END IF;
--    LOGGING.debug('CURSORINTTOMETRICS: Tag Pairs: ' || tagCount || ', TS: ' || BOOL_TO_CHAR(hasTs));
--    
--    FOR i IN 1..tagCount LOOP
--      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
--      LOGGING.debug('Bound Tag Key [' || i || '] at position [' || tId || ']');
--      tId := tId + 1;
--      DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, DBMS_TYPES.TYPECODE_VARCHAR2, 100);
--      --DBMS_SQL.DEFINE_COLUMN(cursorNum, tId, tagV, 100);
--      LOGGING.debug('Bound Tag Value [' || i || '] at position [' || tId || ']');
--      tId := tId + 1;
--    END LOOP;
--    LOGGING.debug('CURSORINTTOMETRICS: Tags Bound');
--    tId := 3;
--    colnum := desctab.first;
--    LOGGING.debug('CURSORINTTOMETRICS: Executing...');
--    --rowsFetched := DBMS_SQL.EXECUTE(cursorNum);
--    LOGGING.debug('CURSORINTTOMETRICS: Executed: ' || rowsFetched);
--    LOOP      
--      rowsFetched := DBMS_SQL.FETCH_ROWS(cursorNum);
--      EXIT WHEN rowsFetched = 0;
--      cntr := cntr + 1;        
--      DBMS_SQL.COLUMN_VALUE(cursorNum, 1, metValue);        
--      DBMS_SQL.COLUMN_VALUE(cursorNum, 2, metName);
--      met := METRIC(metName).HOSTAPPTAGS().VAL(metValue);
--      FOR i IN 1..tagCount LOOP
--        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagK);
--        tId := tId + 1;
--        DBMS_SQL.COLUMN_VALUE(cursorNum, tId, tagV);
--        tId := tId + 1;
--        met := met.PUSHTAG(tagK, tagV);
--      END LOOP;
--      IF(hasTs) THEN
--        CASE tsType
--          WHEN DBMS_TYPES.TYPECODE_TIMESTAMP THEN
--            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestamp);
--            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestamp));
--          WHEN 181 THEN
--            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsTimestampWTimeZone);
--            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, tsTimestampWTimeZone));            
--          WHEN DBMS_TYPES.TYPECODE_DATE THEN
--            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsDate);
--            met := met.ts(TSDB_UTIL.ELAPSEDMS(TSDB_UTIL.EPOCH, TSDB_UTIL.DATETOTIMESTAMP(tsDate)));
--          WHEN DBMS_TYPES.TYPECODE_NUMBER THEN
--            DBMS_SQL.COLUMN_VALUE(cursorNum, colcnt, tsNumber);
--            met := met.ts(tsNumber);
--          END CASE;
--      END IF;
--      tId := 3;
--      metrics.EXTEND();
--      metrics(cntr) := met;
--      --LOGGING.tcplog(met.JSONMS());
--    END LOOP;
--    DBMS_SQL.CLOSE_CURSOR(cursorNum);
--    RETURN metrics;
--    EXCEPTION WHEN OTHERS THEN       
--      DECLARE
--        errm VARCHAR2(200) := SQLERRM();
--      BEGIN
--        LOGGING.error('CURSORINTTOMETRICS ERROR: errm:' || errm);
--        RAISE;                    
--      END;
--      
--      --NULL;
--      
--  END CURSORINTTOMETRICS;

  
  
  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICS(p IN OUT SYS_REFCURSOR) RETURN METRIC_ARR IS
    cursorNum NUMBER;
    a VARCHAR2(1000);
  BEGIN
    SELECT DUMP(p) INTO a FROM DUAL;
    LOGGING.debug('REFCURTOMETRICS [pdump]:[' || a || ']');
  
    cursorNum := DBMS_SQL.TO_CURSOR_NUMBER(p);    
    LOGGING.debug('REFCURTOMETRICS(REF CUR): CURSOR NUM: ' || cursorNum);
    RETURN CURSORINTTOMETRICS(cursorNum);
    EXCEPTION WHEN OTHERS THEN
      BEGIN
        DBMS_SQL.CLOSE_CURSOR(cursorNum);    
        EXCEPTION WHEN OTHERS THEN NULL;
      END;
      DECLARE
        errm VARCHAR2(200) := SQLERRM();
      BEGIN
        LOGGING.error('REFCURTOMETRICS(REF CUR) ERROR: errm:' || errm);
      END;
      RAISE;                    
  END REFCURTOMETRICS;
  
  --====================================================================================================
  -- Attempts to convert the results from the passed ref-cursor to an array of metrics.
  -- Only works, so far as I know, in Oracle 12+
  -- Doc needed
  --====================================================================================================
  FUNCTION REFCURTOMETRICSINONLY(p IN SYS_REFCURSOR) RETURN METRIC_ARR IS
    pout SYS_REFCURSOR := p;
    cursorNum NUMBER;
    a VARCHAR2(1000);
  BEGIN
    RETURN REFCURTOMETRICS(pout);
    EXCEPTION WHEN OTHERS THEN
      BEGIN
        DBMS_SQL.CLOSE_CURSOR(cursorNum);    
        EXCEPTION WHEN OTHERS THEN NULL;
      END;
      DECLARE
        errm VARCHAR2(200) := SQLERRM();
      BEGIN
        LOGGING.error('REFCURTOMETRICSINONLY(REF CUR) ERROR: errm:' || errm);
      END;
      RAISE;                    
  END REFCURTOMETRICSINONLY;
  
  
  --====================================================================================================
  -- Converts the results from the passed SQL query to an array of metrics
  -- Doc needed
  --====================================================================================================
  FUNCTION SQLTOMETRICS(query IN VARCHAR2) RETURN METRIC_ARR IS
    c SYS_REFCURSOR;
    cursorNum NUMBER;
    a VARCHAR2(1000);
  BEGIN
    OPEN c for query;
    RETURN REFCURTOMETRICS(c);
  END SQLTOMETRICS;
  
  
    -- Trace from a ref cursor
  FUNCTION TRACE(p IN RCUR) RETURN INT IS
    metrics METRIC_ARR;
  BEGIN
    metrics := REFCURTOMETRICSINONLY(p);
    IF(metrics IS NOT NULL AND metrics.COUNT > 0) THEN
      IF(LOGGING.IS_DEBUG_ENABLED) THEN
        FOR i in 1..metrics.COUNT LOOP
          LOGGING.debug(metrics(i).JSONMS());
        END LOOP;
      END IF;
      TRACE(metrics);
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





      
*/


  
/


CREATE PUBLIC SYNONYM VARCHAR2_ARR FOR PLTSDB.VARCHAR2_ARR;
CREATE PUBLIC SYNONYM INT_ARR FOR PLTSDB.INT_ARR;
CREATE PUBLIC SYNONYM TSDB_UTIL FOR PLTSDB.TSDB_UTIL;
CREATE PUBLIC SYNONYM LOGGING FOR PLTSDB.LOGGING;
CREATE PUBLIC SYNONYM TAGPAIR FOR PLTSDB.TAGPAIR;
CREATE PUBLIC SYNONYM TAGPAIR_ARR FOR PLTSDB.TAGPAIR_ARR;
CREATE PUBLIC SYNONYM METRIC FOR PLTSDB.METRIC;
CREATE PUBLIC SYNONYM METRIC_ARR FOR PLTSDB.METRIC_ARR;
CREATE PUBLIC SYNONYM METRIC_ARR_ARR FOR PLTSDB.METRIC_ARR_ARR;
CREATE PUBLIC SYNONYM METRIC_EXT FOR PLTSDB.METRIC_EXT;
CREATE PUBLIC SYNONYM METRIC_EXT_ARR FOR PLTSDB.METRIC_EXT_ARR;
CREATE PUBLIC SYNONYM TSDB_TRACER FOR PLTSDB.TSDB_TRACER;
