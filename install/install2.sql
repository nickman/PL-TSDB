--------------------------------------------------------
--  DDL for Table TSDB_OFFLINE_LOGS
--------------------------------------------------------

  CREATE TABLE TSDB_OFFLINE_LOGS (
  	TS TIMESTAMP(6) NOT NULL, 
	SID NUMBER NOT NULL, 
	MESSAGE VARCHAR2(4000) NOT NULL
   );


--------------------------------------------------------
--  DDL for Package LOGGING
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE LOGGING authid definer AS 
  TYPE SOCKET IS TABLE OF utl_tcp.connection INDEX BY VARCHAR2(100);
  procedure error(message IN VARCHAR2);
  procedure log(message IN VARCHAR2);
  procedure debug(message IN VARCHAR2);
  procedure log(level IN PLS_INTEGER, message IN VARCHAR2);
  FUNCTION IS_DEBUG_ENABLED RETURN BOOLEAN;
  FUNCTION WAIT_FOR_ALERT(timeout IN NUMBER DEFAULT 60) RETURN VARCHAR2;
END LOGGING;

/

--------------------------------------------------------
--  DDL for Package Body LOGGING
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE BODY LOGGING AS

  /* An associative array of connections keyed by host:port */
  sockets SOCKET;
  /* The SID of the current session */
  sid NUMBER;


  /* Disconnects the keyed connection */
  PROCEDURE DISCONNECT(key IN VARCHAR2) AS
  BEGIN
    IF sockets.EXISTS(key) THEN 
      BEGIN
        utl_tcp.close_connection(sockets(key));    
        EXCEPTION WHEN OTHERS THEN NULL;  
      END;
      sockets.DELETE(key);
    END IF;  
  END DISCONNECT;
  
  /* Disconnects the specified connection */
  PROCEDURE DISCONNECT(host IN VARCHAR2, port IN PLS_INTEGER) AS
    key VARCHAR2(100) := LOWER(LTRIM(RTRIM(host))) || ':' || port; 
  BEGIN
    DISCONNECT(key);
  END DISCONNECT;
  

  /* Returns a connection to the specified host/port or null if connection fails */
  FUNCTION TCPCONNECT(host IN VARCHAR2 DEFAULT 'localhost', port IN PLS_INTEGER DEFAULT 1234) RETURN utl_tcp.connection AS
    c  utl_tcp.connection;
    key VARCHAR2(100) := LOWER(LTRIM(RTRIM(host))) || ':' || port;
  BEGIN
    IF sockets.EXISTS(key) THEN RETURN sockets(key); END IF;  
    c := utl_tcp.open_connection(remote_host => host,remote_port =>  port,  charset     => 'US7ASCII');  -- open connection
    sockets(key) := c;
    RETURN c;    
    EXCEPTION WHEN OTHERS THEN
      DISCONNECT(key);
      RETURN NULL;
  END TCPCONNECT;
  
  FUNCTION TABLELOG_(message IN VARCHAR2, callingSid IN NUMBER) RETURN PLS_INTEGER IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    INSERT INTO TSDB_OFFLINE_LOGS(TS, SID, MESSAGE) VALUES (SYSTIMESTAMP, callingSid, message);
    COMMIT;
    RETURN 0;
  END TABLELOG_;
  
  PROCEDURE WRITE2CLOB(theClob IN OUT NOCOPY CLOB, content IN VARCHAR2) IS
  BEGIN
    DBMS_LOB.WRITEAPPEND(theClob, length(content), content);      
  END WRITE2CLOB;

  -- Trace all metrics
  PROCEDURE HTTPLOG_(message IN VARCHAR2, logLevel IN PLS_INTEGER) IS
    content CLOB;
    jsonText VARCHAR2(400);
    now TIMESTAMP(9) := SYSTIMESTAMP;
    req   UTL_HTTP.REQ;
    resp  UTL_HTTP.RESP;    
    levelName VARCHAR2(20);
    postUrl VARCHAR2(1000) := 'http://' || TSDB_UTIL.CFG_LOGGING_HTTP_HOST || ':' || TSDB_UTIL.CFG_LOGGING_HTTP_PORT || TSDB_UTIL.CFG_LOGGING_HTTP_URI;
    retCode INT;
  BEGIN
    levelName := CASE logLevel
      WHEN 1 THEN 'ERROR'
      WHEN 2 THEN 'INFO'
      WHEN 3 THEN 'DEBUG'
      ELSE 'UNKNOWN-' || logLevel
    END;      
    DBMS_LOB.CREATETEMPORARY(content, true, DBMS_LOB.CALL);
    DBMS_LOB.OPEN(content, DBMS_LOB.LOB_READWRITE);
    WRITE2CLOB(content, '{"time":"' || TO_CHAR(now, TSDB_UTIL.CFG_LOGGING_TS_FORMAT) || '","level":"' || levelName || '","sid":' || SID || '","message":"');
    WRITE2CLOB(content,message);
    WRITE2CLOB(content,'"}');
    req := UTL_HTTP.BEGIN_REQUEST (url=> postUrl, method => 'POST');
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
    DBMS_LOB.CLOSE(content);
    DBMS_LOB.FREETEMPORARY(content);    
    EXCEPTION WHEN OTHERS THEN 
        DECLARE
          errm VARCHAR2(200) := SQLERRM();
        BEGIN
          BEGIN
            DBMS_LOB.CLOSE(content);
            DBMS_LOB.FREETEMPORARY(content);    
            EXCEPTION WHEN OTHERS THEN NULL;
          END;
          retCode := TABLELOG_(message, SID);          
          retCode := TABLELOG_('TRACE ERROR: errm:' || errm, SID);
        END;
  END HTTPLOG_;
  

  FUNCTION TCPLOG_(message IN VARCHAR2) RETURN PLS_INTEGER IS
    ret_val pls_integer; 
    c  utl_tcp.connection;
    host VARCHAR2(300) := TSDB_UTIL.CFG_LOGGING_TELNET_HOST;
    port PLS_INTEGER := TSDB_UTIL.CFG_LOGGING_TELNET_PORT;  
  BEGIN
    c := TCPCONNECT(host, port);
    RETURN TABLELOG_(message, SID);
    EXCEPTION WHEN OTHERS THEN
      DISCONNECT(host, port);    
      RETURN TABLELOG_(message, SID);          
  END TCPLOG_;

  FUNCTION ALERTLOG_(message IN VARCHAR2) RETURN PLS_INTEGER IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    aname VARCHAR2(30) := TSDB_UTIL.CFG_LOGGING_ALERT_NAME;
    ret_val pls_integer; 
  BEGIN
    DBMS_ALERT.SIGNAL(aname, message);
    COMMIT;
--    DBMS_OUTPUT.PUT_LINE('ALERT SENT: [' || aname || ']');
    ret_val := TABLELOG_('ALERT SENT: [' || aname || ']', SID);
    RETURN 0;
  END ALERTLOG_;
  
  FUNCTION WAIT_FOR_ALERT(timeout IN NUMBER DEFAULT 60) RETURN VARCHAR2 IS
    PRAGMA AUTONOMOUS_TRANSACTION;
    name CONSTANT VARCHAR2(30) := TSDB_UTIL.CFG_LOGGING_ALERT_NAME;
    message VARCHAR2(1800);
    status INT;
  BEGIN
    DBMS_ALERT.REGISTER (name);
    DBMS_OUTPUT.PUT_LINE('WAITING ON ALERT FROM: [' || name || ']');
    DBMS_ALERT.WAITONE (name, message, status, timeout);
    IF(status=0) THEN
      DBMS_ALERT.REMOVE(name);
      COMMIT;
      RETURN message;
    END IF;
    DBMS_ALERT.REMOVE(name);
    COMMIT;
    RETURN NULL;    
    EXCEPTION WHEN OTHERS THEN
      BEGIN
        DBMS_ALERT.REMOVE(name);
        EXCEPTION WHEN OTHERS THEN NULL;
      END;
  END WAIT_FOR_ALERT;
  
  

  PROCEDURE DOLOG_(message IN VARCHAR2) IS
    logType VARCHAR2(20) := TSDB_UTIL.CFG_LOGGING_TYPE;
    result PLS_INTEGER := 0;
    ts VARCHAR2(60) := '[' || TO_CHAR(SYSTIMESTAMP, TSDB_UTIL.CFG_LOGGING_TS_FORMAT) || ']('  || SID || '):';
  BEGIN
    result := CASE TSDB_UTIL.CFG_LOGGING_TYPE
      WHEN TSDB_UTIL.LOG_TABLE THEN 
        TABLELOG_(ts || message, SID)
      WHEN TSDB_UTIL.LOG_ALERT THEN 
        ALERTLOG_(ts || message)
      WHEN TSDB_UTIL.LOG_TCP THEN 
        TCPLOG_(ts || message)
      ELSE
        TABLELOG_(ts || message, SID)
    END;
  END DOLOG_;
  
  FUNCTION IS_DEBUG_ENABLED RETURN BOOLEAN IS
  BEGIN
    RETURN TSDB_UTIL.IS_LOG_ENABLED(TSDB_UTIL.LEVEL_ALL);
  END IS_DEBUG_ENABLED;
  

  procedure error(message IN VARCHAR2) IS
  BEGIN
    IF(TSDB_UTIL.IS_LOG_ENABLED(TSDB_UTIL.LEVEL_ERROR)) THEN
      IF(TSDB_UTIL.CFG_LOGGING_TYPE = TSDB_UTIL.LOG_HTTP) THEN
        HTTPLOG_(message, TSDB_UTIL.LEVEL_ERROR);
        RETURN;
      END IF;
      DOLOG_('ERROR:' || message || '. backtrace:' || dbms_utility.format_error_backtrace);
    END IF;
  END error;
  
  procedure log(message IN VARCHAR2) IS
  BEGIN
    IF(TSDB_UTIL.IS_LOG_ENABLED(TSDB_UTIL.LEVEL_INFO)) THEN
      IF(TSDB_UTIL.CFG_LOGGING_TYPE = TSDB_UTIL.LOG_HTTP) THEN
        HTTPLOG_(message, TSDB_UTIL.LEVEL_ERROR);
        RETURN;
      END IF;
      DOLOG_('INFO:' || message);
    END IF;
  END log;
  
  procedure debug(message IN VARCHAR2) IS
  BEGIN
    IF(TSDB_UTIL.IS_LOG_ENABLED(TSDB_UTIL.LEVEL_ALL)) THEN
      IF(TSDB_UTIL.CFG_LOGGING_TYPE = TSDB_UTIL.LOG_HTTP) THEN
        HTTPLOG_(message, TSDB_UTIL.LEVEL_ERROR);
        RETURN;
      END IF;
      DOLOG_('DEBUG:' || message);
    END IF;    
  END debug;
  
  procedure log(level IN PLS_INTEGER, message IN VARCHAR2) IS
    prefix VARCHAR2(30);
  BEGIN    
    IF(TSDB_UTIL.IS_LOG_ENABLED(level)) THEN
      IF(TSDB_UTIL.CFG_LOGGING_TYPE = TSDB_UTIL.LOG_HTTP) THEN
        HTTPLOG_(message, level);
        RETURN;
      END IF;        
      prefix := CASE level 
        WHEN 1 THEN 'ERROR'
        WHEN 2 THEN 'INFO'
        WHEN 3 THEN 'DEBUG'
        ELSE 'UNKOWN-' || level
      END;
      DOLOG_(prefix || ':' || message);
    END IF;    
  END log;
  

  BEGIN
    SELECT SYS_CONTEXT('USERENV', 'SID') INTO sid FROM DUAL;        
END LOGGING;

/
