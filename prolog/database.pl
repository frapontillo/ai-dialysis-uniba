% connect to the database
connect :- odbc_connect('DialysisAIx64', _, [ user(postgres), password(postgres), alias(dialysisAI), open(once) ]), odbc_current_connection(X, _), write('Connected to '), write(X), nl.
% disconnect from the database
disconnect :- odbc_current_connection(X, _), odbc_disconnect(X), write('Disconnected from '), write(X), nl.

% get the first 10 rowsb
session_db(X) :- odbc_query(dialysisAI, 'SELECT "ID", "PATIENT", "CENTER", "PATIENT_SEX", "PATIENT_AGE", "PATIENT_RACE" FROM "PATIENT_DIALYSIS_SYMPTOM" LIMIT 10', X).