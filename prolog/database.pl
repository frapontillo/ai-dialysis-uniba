% read the location of the config file
get_config_path(ConfigPath) :-
	println([
		'Type the name of the connection config file in the config directory.\n',
		'Remember to use single quotes (e.g. ''my-connection.properties'').']),
	read_term(ConfigFile, [syntax_errors(quiet)]),
	string_concat('config/', ConfigFile, ConfigPathString),
	atom_string(ConfigPath, ConfigPathString).
% read database parameters with fallback on 'config/database.properties'
read_database_params(Driver, Server, Port, Database, User, Password) :-
	log_v('database','Asking for the config path.'),
	get_config_path(ConfigPath),
	log_v('database','Trying to get access to the config file.'),
	access_file(ConfigPath, read),
	log_i('database', 'WIN: yo, the config file exists and can be read.'),
	read_database_params(ConfigPath, Driver, Server, Port, Database, User, Password), !.
read_database_params(Driver, Server, Port, Database, User, Password) :-
	log_w('database', 'FAIL: could not read custom parameter config file, falling back to database.properties.'),
	read_database_params('config/database.properties', Driver, Server, Port, Database, User, Password).
% read database parameters from a given file
read_database_params(ConfigPath, Driver, Server, Port, Database, User, Password) :-
	new_table(ConfigPath, [ attribute(atom), value(atom) ], [ field_separator(61), record_separator(10) ], PropertiesFile),
	open_table(PropertiesFile),
	log_v('database','Reading database parameters...'),
	read_database_param(PropertiesFile, 0),
	close_table(PropertiesFile),
	db_param(driver, Driver), db_param(server, Server), db_param(port, Port), db_param(database, Database), db_param(username, User), db_param(password, Password),
	log_i('database', 'Database parameters read.').
% iterate over parameters to read
read_database_param(PropertiesFile, Row) :-
	% if there is another record
	not(Row = -1),
	% read it and parse it
	read_table_fields(PropertiesFile, Row, Next, [attribute(Attribute), value(Value)]),
	% print it
	% println([Attribute, '=', Value]),
	% save it
	assertz(db_param(Attribute, Value)),
	% get the next record
	read_database_param(PropertiesFile, Next), !.
% always satisfy
read_database_param(_, _).
% connect to the database
connect :-
	read_database_params(Driver, Server, Port, Database, User, Password),
	concat_string_list([ 'DRIVER=', Driver, ';', 'SERVER=', Server, ';', 'PORT=', Port, ';', 'DATABASE=', Database, ';', 'USER=', User, ';', 'PASSWORD=', Password, ';' ], ConnectionString),
	odbc_driver_connect(ConnectionString, _, [ alias(dialysis_connection) ]),
	log_i('database', ['Connected to ', dialysis_connection]).
% disconnect from the database
disconnect :- odbc_current_connection(dialysis_connection, _), odbc_disconnect(dialysis_connection), println(['Disconnected from ', dialysis_connection]).
% check if it is connected
is_connected :- odbc_current_connection(dialysis_connection, _).

% ------------------ %
%      SYMPTOMS      %
% ------------------ %
% 1st arg: ID        %
% 2nd arg: Attr name %
% 3rd arg: Attr val  %
% ------------------ %
:- dynamic symptom/3.
% clear the symptom facts
clear_symptoms :- retractall(symptom(_,_,_)).
% assert the symptom facts
get_symptoms :-
	log_v('database', ['Fetching symptoms...']),
	measure_time,
	odbc_query(dialysis_connection, 'SELECT `symptom`.`ID`, `symptom`.`DESCRIPTION` FROM `dialysisai`.`symptom`;', row(ID, Description)),
	assertz(symptom(ID, 'ID', ID)),
	assertz(symptom(ID, 'Description', Description)),
	fail.
get_symptoms :-
	measure_time(Time), count_symptoms(Count), format_ms(Time, TimeString),
	log_i('database', [Count, ' symptoms fetched in ', TimeString, '.']).
% print the symptom facts
print_symptoms :- symptom(ID, 'Description', Description), println([ID, ':', Description]).
% clear, updates and prints the symptom facts
update_symptoms :- clear_symptoms, get_symptoms.
% check if the given symptom ID exists
exists_symptom(ID) :- symptom(ID, 'ID', ID).
% count the number of symptoms
count_symptoms(Count) :- aggregate_all(count, symptom(ID, 'ID', ID), Count).


% ------------------ %
%       RECORDS      %
% ------------------ %
% 1st arg: ID        %
% 2nd arg: Attr name %
% 3rd arg: Attr val  %
% ------------------ %
% clear the record facts
clear_records(RecordName) :- RecordList =..[RecordName, _, _, _], retractall(RecordList).
% fetch next row
save_records(Statement, RecordName) :-
	odbc_fetch(Statement, Row, next), 
	(
		Row == end_of_file ->
			true ;
			Row = row(ID, Patient, Center, PatientSex, PatientRace, PatientAge, SessionID, SessionDate,
			KTV, QB, ProgWeightLoss, RealWeightLoss, DeltaWeight, ProgDuration, RealDuration, DeltaDuration,
			SAPStart, SAPEnd, SAPAverage, DAPStart, DAPEnd, DAPAverage, BloodVolume, DeltaBloodFlow, DeltaUF,
			SymptomID, Score),
			IDList =..[RecordName, ID, 'ID', ID], assertz(IDList),
			PatientList =..[RecordName, ID, 'Patient', Patient], assertz(PatientList),
			CenterList =..[RecordName, ID, 'Center', Center], assertz(CenterList),
			PatientSexList =..[RecordName, ID, 'PatientSex', PatientSex], assertz(PatientSexList),
			PatientRaceList =..[RecordName, ID, 'PatientRace', PatientRace], assertz(PatientRaceList),
			PatientAgeList =..[RecordName, ID, 'PatientAge', PatientAge], assertz(PatientAgeList),
			SessionIDList =..[RecordName, ID, 'SessionID', SessionID], assertz(SessionIDList),
			SessionDateList =..[RecordName, ID, 'SessionDate', SessionDate], assertz(SessionDateList),
			KTVList =..[RecordName, ID, 'KTV', KTV], assertz(KTVList),
			QBList =..[RecordName, ID, 'QB', QB], assertz(QBList),
			ProgWeightLossList =..[RecordName, ID, 'ProgWeightLoss', ProgWeightLoss], assertz(ProgWeightLossList),
			RealWeightLossList =..[RecordName, ID, 'RealWeightLoss', RealWeightLoss], assertz(RealWeightLossList),
			DeltaWeightList =..[RecordName, ID, 'DeltaWeight', DeltaWeight], assertz(DeltaWeightList),
			ProgDurationList =..[RecordName, ID, 'ProgDuration', ProgDuration], assertz(ProgDurationList),
			RealDurationList =..[RecordName, ID, 'RealDuration', RealDuration], assertz(RealDurationList),
			DeltaDurationList =..[RecordName, ID, 'DeltaDuration', DeltaDuration], assertz(DeltaDurationList),
			SAPStartList =..[RecordName, ID, 'SAPStart', SAPStart], assertz(SAPStartList),
			SAPEndList =..[RecordName, ID, 'SAPEnd', SAPEnd], assertz(SAPEndList),
			SAPAverageList =..[RecordName, ID, 'SAPAverage', SAPAverage], assertz(SAPAverageList),
			DAPStartList =..[RecordName, ID, 'DAPStart', DAPStart], assertz(DAPStartList),
			DAPEndList =..[RecordName, ID, 'DAPEnd', DAPEnd], assertz(DAPEndList),
			DAPAverageList =..[RecordName, ID, 'DAPAverage', DAPAverage], assertz(DAPAverageList),
			BloodVolumeList =..[RecordName, ID, 'BloodVolume', BloodVolume], assertz(BloodVolumeList),
			DeltaBloodFlowList =..[RecordName, ID, 'DeltaBloodFlow', DeltaBloodFlow], assertz(DeltaBloodFlowList),
			DeltaUFList =..[RecordName, ID, 'DeltaUF', DeltaUF], assertz(DeltaUFList),
			SymptomIDList =..[RecordName, ID, 'SymptomID', SymptomID], assertz(SymptomIDList),
			ScoreList =..[RecordName, ID, 'Score', Score], assertz(ScoreList),
			save_records(Statement, RecordName)
	).
% assert the record facts
get_records(SymptomID, RecordName) :-
	measure_time,
	log_v('database', ['Fetching records for symptom ', SymptomID, '...']),
	log_v('database', ['Preparing statement...']),
	% prepare the statement
	odbc_prepare(dialysis_connection, 
		'SELECT `ID`,\c
    `PATIENT`, `CENTER`, `PATIENT_SEX`, `PATIENT_RACE`, `PATIENT_AGE`, `SESSION_ID`, `SESSION_DATE`,\c
    `KTV`, `QB`, `PROG_WEIGHT_LOSS`, `REAL_WEIGHT_LOSS`, `DELTA_WEIGHT`, `PROG_DURATION`, `REAL_DURATION`, `DELTA_DURATION`,\c
    `SAP_START`, `SAP_END`, `AVG_SAP`, `DAP_START`, `DAP_END`, `AVG_DAP`, `BLOOD_VOLUME`, `DELTA_BLOOD_FLOW`, `DELTA_UF`,\c
    `SYMPTOM_ID`, `SCORE`\c
		FROM `dialysisai`.`patient_dialysis_symptom_for_analysis` WHERE `SYMPTOM_ID` = ? ORDER BY `SCORE` DESC LIMIT 1000;',
		[default], Statement, [fetch(fetch)]),
	log_v('database', ['Statement prepared.']),
	% execute the statement
	odbc_execute(Statement, [SymptomID], _TheRow),
	% loop over the records and store them
	save_records(Statement, RecordName),
	measure_time(Time), count_records(RecordName, CountRecords), format_ms(Time, TimeString), log_i('database', [CountRecords, ' records fetched in ', TimeString, '.']).
% print the record facts
print_records(RecordName) :- _RecordList =..[RecordName, ID, Attribute, Value], println([ID, '.', Attribute, ' = ', Value]), fail; true.
% clear, updates and prints the record facts
update_records :-
	clear_records(negative),			% clear negative records
	clear_records(positive),			% clear positive records
	get_records('1', negative),		% get negative records
	get_records('2', positive).		% get positive records
% check if the given record ID exists
exists_record(RecordName, ID) :- RecordList =..[RecordName, ID, 'ID', ID], RecordList.
% count the number of records
count_records(RecordName, Count) :- RecordList =..[RecordName, ID, 'ID', ID], aggregate_all(count, RecordList, Count).

% ------------------ %
%      ACCESSORY     %
% ------------------ %
example(positive, ID, Attribute, Value) :- positive(ID, Attribute, Value).
example(negative, ID, Attribute, Value) :- negative(ID, Attribute, Value).
