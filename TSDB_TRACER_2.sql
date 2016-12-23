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

  TYPE METRIC_REC IS RECORD (
    METRIC_ID NUMBER,
    XROWID VARCHAR2(18),
    M METRIC
  );
  TYPE QCUR IS REF CURSOR RETURN METRIC_REC;
  
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
  
  -- *******************************************************
  --    Get current XID function
  -- *******************************************************
  FUNCTION CURRENTXID RETURN RAW;  

END TSDB_TRACER;

/
