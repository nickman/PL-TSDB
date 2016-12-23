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
