CREATE USER PLTSDB IDENTIFIED BY tpl;
GRANT DBA TO PLTSDB;
GRANT EXECUTE ON DBMS_LOCK TO PLTSDB;
GRANT EXECUTE ON DBMS_TRANSACTION to PLTSDB;
GRANT SELECT ON V_$TRANSACTION TO PLTSDB;
GRANT SELECT ON v_$MYSTAT to PLTSDB;
GRANT SELECT ON v_$STATNAME to PLTSDB;
GRANT SELECT ON v_$OSSTAT to PLTSDB;
GRANT EXECUTE ON DBMS_ALERT TO PLTSDB;
GRANT EXECUTE ON DBMS_APPLICATION_INFO TO PLTSDB;
GRANT EXECUTE ON DBMS_SQL TO PLTSDB;
GRANT EXECUTE ON DBMS_SESSION TO PLTSDB;
