% read database parameters
read_database_params :-
	new_table('database.properties', [ attribute(string), value(string) ], [field_separator(61), record_separator(10)], PropertiesFile),
	open_table(PropertiesFile),
	println('Reading database parameters...'),
	read_database_param(PropertiesFile, 0),
	close_table(PropertiesFile),
	println('Database parameters read.'), nl.
% iterate over parameters to read
read_database_param(PropertiesFile, Row) :-
	% if there is another record
	not(Row = -1),
	% read it and parse it
	read_table_fields(PropertiesFile, Row, Next, [attribute(Attribute), value(Value)]),
	% print it
	println([Attribute, '=', Value]),
	% save it
	assertz(database_param(Attribute, Value)),
	% get the next record
	read_database_param(PropertiesFile, Next), !.
% always satisfy
read_database_param(PropertiesFile, Row).
% connect to the database
connect :-
	read_database_params,
	odbc_driver_connect( 'DRIVER={MySQL ODBC 5.2 ANSI Driver};SERVER=localhost;PORT=3306;DATABASE=dialysisai;USER=root;PASSWORD=mysql;', _, [ alias(dialysis_connection) ]),
	println(['Connected to ', dialysis_connection]).
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
	println(['Fetching symptoms...']),
	measure_time,
	odbc_query(dialysis_connection, 'SELECT `symptom`.`ID`, `symptom`.`DESCRIPTION` FROM `dialysisai`.`symptom`;', row(ID, Description)),
	assertz(symptom(ID, 'ID', ID)),
	assertz(symptom(ID, 'Description', Description)),
	fail.
get_symptoms :-
	measure_time(Time), count_symptoms(Count), println([Count, ' symptoms fetched in ', Time, ' ms']), nl.
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
			PatientList =..[RecordName, ID, 'Center', Patient], assertz(PatientList),
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
	println(['Fetching records for symptom ', SymptomID, '...']),
	println(['Preparing statement...']),
	% prepare the statement
	odbc_prepare(dialysis_connection, 
		'SELECT `ID`,\c
    `PATIENT`, `CENTER`, `PATIENT_SEX`, `PATIENT_RACE`, `PATIENT_AGE`, `SESSION_ID`, `SESSION_DATE`,\c
    `KTV`, `QB`, `PROG_WEIGHT_LOSS`, `REAL_WEIGHT_LOSS`, `DELTA_WEIGHT`, `PROG_DURATION`, `REAL_DURATION`, `DELTA_DURATION`,\c
    `SAP_START`, `SAP_END`, `AVG_SAP`, `DAP_START`, `DAP_END`, `AVG_DAP`, `BLOOD_VOLUME`, `DELTA_BLOOD_FLOW`, `DELTA_UF`,\c
    `SYMPTOM_ID`, `SCORE`\c
		FROM `dialysisai`.`patient_dialysis_symptom_for_analysis` WHERE `SYMPTOM_ID` = ? ORDER BY `SCORE` DESC;',
		[default], Statement, [fetch(fetch)]),
	println(['Statement prepared.']),
	% execute the statement
	odbc_execute(Statement, [SymptomID], _TheRow),
	% loop over the records and store them
	save_records(Statement, RecordName),
	measure_time(Time), count_records(RecordName, CountRecords), println([CountRecords, ' records fetched in ', Time, ' ms']), nl.
% print the record facts
print_records :- record(ID, Attribute, Value), println([ID, '.', Attribute, ' = ', Value]), fail; true.
% clear, updates and prints the record facts
update_records :- clear_records(negative), clear_records(positive), get_records('1', negative), get_records('2', positive).
% check if the given record ID exists
exists_record(RecordName, ID) :- RecordList =..[RecordName, ID, 'ID', ID], RecordList.
% count the number of records
count_records(RecordName, Count) :- RecordList =..[RecordName, ID, 'ID', ID], aggregate_all(count, RecordList, Count).