SELECT count(query)
FROM pg_stat_activity
WHERE datname='whoop'
  AND NOT (STATE='idle'
           OR pid=pg_backend_pid())
  AND waiting = 't';
