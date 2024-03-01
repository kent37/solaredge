COPY Dates FROM 'solaredge.duckdb.0.8.1/dates.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY Energy FROM 'solaredge.duckdb.0.8.1/energy.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
COPY Power FROM 'solaredge.duckdb.0.8.1/power.csv' (FORMAT 'csv', quote '"', delimiter ',', header 0);
