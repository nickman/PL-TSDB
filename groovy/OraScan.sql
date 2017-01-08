import oracle.jdbc.pool.OracleDataSource;
import groovy.sql.*;
import java.sql.*;
import oracle.jdbc.*;
import oracle.jdbc.aq.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import static oracle.jdbc.OracleConnection.*;

String DRIVER = "oracle.jdbc.OracleDriver";
// String URL = "jdbc:oracle:oci8:@";
//String URL = "jdbc:oracle:thin:@//localhost:1521/XE";
//String URL = "jdbc:oracle:thin:@//192.168.1.35:1521/ORCL";
//String URL = "jdbc:oracle:thin:@//localhost:1521/ORCL";
//String URL = "jdbc:oracle:thin:@//localhost:1522/ORCL";
String URL = "jdbc:oracle:thin:@//192.168.1.189:1521/ORCL";
//String URL = "jdbc:oracle:thin:@//tpmint:1521/ORCL";
String USER = "PLTSDB";
String PASS = "plt";

final String CLIENT_INFO = "OraScanClient";
final String MODULE = "OraScan";
final String ACTION = "Scanning";


ds = new OracleDataSource();
ds.setDriverType(DRIVER);
ds.setURL(URL);
ds.setUser(USER);
ds.setPassword(PASS);
sql = Sql.newInstance(ds);

systemWaitsTime = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TIME_WAITED) * 10 TOTAL_TIME_WAITED, 'ora.system.timewaited.all', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemWaitsTimeMicros = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TIME_WAITED_MICRO) TOTAL_TIME_WAITED_MICROS, 'ora.system.microswaited.all', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemWaitsCount = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TOTAL_WAITS) * 10 TOTAL_WAITS, 'ora.system.waitcount.all', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemTimeoutsCount = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TOTAL_TIMEOUTS) * 10 TOTAL_WAITS, 'ora.system.timeouts.all', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
//----
systemWaitsTimeFg = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TIME_WAITED_FG) * 10 TOTAL_TIME_WAITED, 'ora.system.timewaited.fg', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemWaitsTimeMicrosFg = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TIME_WAITED_MICRO_FG) TOTAL_TIME_WAITED_MICROS, 'ora.system.microswaited.fg', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemWaitsCountFg = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TOTAL_WAITS_FG) * 10 TOTAL_WAITS, 'ora.system.waitcount.fg', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemTimeoutsCountFg = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    SELECT SUM(E.TOTAL_TIMEOUTS_FG) * 10 TOTAL_WAITS, 'ora.system.timeouts.fg', 'wait_type' , TSDB_UTIL.CLEAN(E.WAIT_CLASS, '_')
    FROM V\$SYSTEM_EVENT E
    WHERE E.WAIT_CLASS != 'Idle'
    GROUP BY E.WAIT_CLASS
)))) X FROM DUAL
""";
systemStats = """
SELECT TSDB_TRACER.TRACE(TSDB_TRACER.APPINFOTAGS(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(
    select VALUE, 'ora.system.stat.' || TSDB_UTIL.CLEAN(TSDB_TRACER.DECODE_CLASS(CLASS)) || '.' ||  TSDB_UTIL.CLEAN(name) 
    FROM V\$SYSSTAT WHERE TSDB_TRACER.DECODE_CLASS(CLASS) != 'Debug' 
)))) X FROM DUAL
""";


traceQuery = { query ->
    return """SELECT TSDB_TRACER.TRACE(TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(${query}))) X FROM DUAL""";
}

aggQuery = { query ->
    return """TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR(${query}))""";
}

// SELECT TSDB_TRACER.TRACE(TSDB_TRACER.PACK(METRIC_ARR_ARR(
//     TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR()),
//     TSDB_TRACER.REFCURTOMETRICSINONLY(CURSOR())    
// ))) X FROM DUAL;




traceQueries = { queries ->
    int q = 0;
    final StringBuilder b = new StringBuilder("SELECT TSDB_TRACER.TRACE(TSDB_TRACER.PACK(METRIC_ARR_ARR(");
    queries.each() { sc ->
        b.append(aggQuery(sc)).append(",");
        q++;
    }
    if(q > 1) {
        b.deleteCharAt(b.length()-1);
    }
    b.append("))) X FROM DUAL");
    return b.toString();
}

scripts = [
    systemWaitsTime : "SYSTEMWAITSTIME",
    systemWaitsCount : "SYSTEMWAITSCOUNT",
    systemWaitsTimeMicros : "SYSTEMWAITSTIMEMICROS",
    systemTimeoutsCount : "SYSTEMTIMEOUTSCOUNT",
    systemWaitsTimeFg : "SYSTEMWAITSTIMEFG",
    systemWaitsCountFg : "SYSTEMWAITSCOUNTFG",
    systemWaitsTimeMicrosFg : "SYSTEMWAITSTIMEMICROSFG",
    systemTimeoutsCountFg : "SYSTEMTIMEOUTSCOUNTFG",
    systemStats : "SYSTEMSTATS",
]


final String[] end2End = new String[END_TO_END_STATE_INDEX_MAX];
final short ix = 0;
end2End[END_TO_END_CLIENTID_INDEX] = CLIENT_INFO;
end2End[END_TO_END_MODULE_INDEX] = MODULE;
end2End[END_TO_END_ACTION_INDEX] = ACTION;

conn = null;
ps = null;
rset = null;

try {
    conn = ds.getConnection();
    conn.setEndToEndMetrics(end2End, ix);
    while(true) {
        long start = System.currentTimeMillis();
        metrics = 0;
        scripts.each() { scr, scrName ->
            try {
                end2End[END_TO_END_ACTION_INDEX] = scrName;
                conn.setEndToEndMetrics(end2End, ix);            
                ps = conn.prepareStatement(getProperty(scr));
                rset = ps.executeQuery();
                rset.next();
                metrics += rset.getInt(1);
            } finally {
                if(rset!=null) try { rset.close();} catch (x) {}
                if(ps!=null) try { ps.close(); } catch (x) {}
            }
        }
        long elapsed = System.currentTimeMillis() - start;
        println "Collected $metrics in $elapsed ms.";        
        Thread.sleep(5000);
        // sql.call("BEGIN DBMS_APPLICATION_INFO.SET_CLIENT_INFO(?); DBMS_APPLICATION_INFO.SET_MODULE(?,?); END;", [CLIENT_INFO, MODULE, ACTION], {});
        // long appInfoElapsed = System.currentTimeMillis() - start;
        // println "AppInfo set in $appInfoElapsed ms.";
    }    
} finally {
    if(conn!=null) try { conn.close(); println "Conn Closed"; } catch (x) {}
}

