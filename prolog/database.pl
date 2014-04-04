/** 
 * <module> database
 *
 * Use this module to connect to the database and fetch all of the relevant facts.
 *
 * @author Francesco Pontillo
 * @license Apache License, Version 2.0
*/

% --------------------------------------------------------------------------- %
%                               DATABASE PARAMETERS                           %
% --------------------------------------------------------------------------- %

/**
 * db_param(?Name, ?Value) is semidet.
 *
 * Holds database parameters in the form `db_param(Name, Value)`.
 * Relevant names are: server, port, database, username, password.
 */
:- dynamic db_param/2.

/**
 * get_config_path(-ConfigPath) is det.
 *
 * Ask the user to insert the configuration file name, to be found under the 'config' folder.
 *
 * @param ConfigPath        The name of the database configuration file. The file has to be in the
 *                          '.properties' format.
 */
% read the location of the config file
get_config_path(ConfigPath) :-
    println([
        'Type the name of the connection config file in the config directory.\n',
        'Remember to use single quotes (e.g. ''my-connection.properties'').']),
    read_term(ConfigFile, [syntax_errors(quiet)]),
    string_concat('config/', ConfigFile, ConfigPathString),
    atom_string(ConfigPath, ConfigPathString).

/**
 * read_database_params(-Driver, -Server, -Port, -Database, -User, -Password) is semidet.
 *
 * Retrieves all the database connection parameters from a config file whose name is entered
 * by the user when requested.
 * If the user-entered configuration file doesn't exist, fall back on 'config/database.properties'.
 *
 * @param Driver            The ODBC driver name.
 * @param Server            The server address.
 * @param Port              The server port.
 * @param Database          The database to connect to.
 * @param User              The username to use.
 * @param Password          The password to use.
 */
read_database_params(Driver, Server, Port, Database, User, Password) :-
    log_v('database','Asking for the config path.'),
    get_config_path(ConfigPath),
    log_v('database','Trying to get access to the config file.'),
    access_file(ConfigPath, read),
    log_i('database', 'WIN: yo, the config file exists and can be read.'),
    read_database_params(ConfigPath, Driver, Server, Port, Database, User, Password), !.
read_database_params(Driver, Server, Port, Database, User, Password) :-
    log_w('database', [
        'FAIL: could not read custom parameter config file, ',
        'falling back to database.properties.']),
    read_database_params('config/database.properties', 
        Driver, Server, Port, Database, User, Password).

/**
 * read_database_params(+Path, -Driver, -Server, -Port, -Database, -User, -Password) is semidet.
 *
 * Retrieves all the database connection parameters from a given config file.
 *
 * @param Path              The database configuration file name.
 * @param Driver            The ODBC driver name.
 * @param Server            The server address.
 * @param Port              The server port.
 * @param Database          The database to connect to.
 * @param User              The username to use.
 * @param Password          The password to use.
 */
read_database_params(Path, Driver, Server, Port, Database, User, Password) :-
    retractall(db_param(_,_)),
    new_table(
        Path, 
        [ attribute(atom), value(atom) ], [ field_separator(61), record_separator(10) ], 
        PropertiesFile),
    open_table(PropertiesFile),
    log_v('database','Reading database parameters...'),
    read_database_param(PropertiesFile, 0),
    close_table(PropertiesFile),
    db_param(driver, Driver), db_param(server, Server), db_param(port, Port),
    db_param(database, Database), db_param(username, User), db_param(password, Password),
    log_i('database', 'Database parameters read.').

/**
 * read_database_param(+PropertiesFile, +Row) is det.
 *
 * Read database params from an open '.properties' file at the given index.
 * Every read property is asserts into the Prolog in-memory database.
 * It always iterates until the reading is over. Then, always succeeds.
 *
 * @param PropertiesFile    An open '.properties' file.
 * @param Row               The row to start reading from.
 */
read_database_param(PropertiesFile, Row) :-
    % if there is another record
    not(Row = -1),
    % read it and parse it
    read_table_fields(PropertiesFile, Row, Next, [attribute(Attribute), value(Value)]),
    % save it
    assertz(db_param(Attribute, Value)),
    % get the next record
    read_database_param(PropertiesFile, Next), !.
% always satisfy
read_database_param(_, _).

% --------------------------------------------------------------------------- %
%                               DATABASE CONNECTION                           %
% --------------------------------------------------------------------------- %

/**
 * connect(+ConfigPath) is det.
 *
 * Connect to the database by relying on the ConfigPath variable.
 *
 * @param ConfigPath        The name of the database configuration file to be found in the
 *                          'config' folder, or:
 *                            - default, use the 'config/database.properties' file
 *                            - ask, prompt the user for a file name
 */
% connect to the database by using the user provided config file or the default one
connect(default) :-
    log_d('connect', 'Using default connection configuration.'),
    connect('config/database.properties'), !.
% connect to the database by asking the user for a configuration file
connect(ask) :-
    log_d('connect', 'Asking for database configuration.'),
    read_database_params(Driver, Server, Port, Database, User, Password),
    connect(Driver, Server, Port, Database, User, Password), !.
% default behavior, use the provided config file
connect(ConfigPath) :-
    log_d('connect', 'Using a configuration file.'),
    read_database_params(ConfigPath, Driver, Server, Port, Database, User, Password),
    connect(Driver, Server, Port, Database, User, Password).

/**
 * connect(-Driver, -Server, -Port, -Database, -User, -Password) is semidet.
 *
 * Try to connect to the database by using the provided prameters.
 *
 * @param Driver            The ODBC driver name.
 * @param Server            The server address.
 * @param Port              The server port.
 * @param Database          The database to connect to.
 * @param User              The username to use.
 * @param Password          The password to use.
 */
connect(Driver, Server, Port, Database, User, Password) :-
    concat_string_list([ 
        'DRIVER=', Driver, ';', 'SERVER=', Server, ';', 'PORT=', Port, ';', 
        'DATABASE=', Database, ';', 'USER=', User, ';', 'PASSWORD=', Password, ';' ], 
        ConnectionString),
    odbc_driver_connect(ConnectionString, _, [ alias(dialysis_connection) ]),
    log_i('database', ['Connected to ', dialysis_connection]).

/**
 * disconnect is semidet.
 *
 * If there is an open connection, disconnects from it.
 */
disconnect :-
    odbc_current_connection(dialysis_connection, _),
    odbc_disconnect(dialysis_connection),
    log_d('database', ['Disconnected from ', dialysis_connection]).

/**
 * is_connected is semidet
 *
 * Check if there is a currently open connection.
 */
is_connected :- odbc_current_connection(dialysis_connection, _).

% --------------------------------------------------------------------------- %
%                                    SYMPTOMS                                 %
% --------------------------------------------------------------------------- %

/**
 * symptom(?ID, ?Attribute, ?Value) is semidet.
 *
 * Holds information for all retrieved symptoms.
 *
 * @param ID                The ID of the symptom.
 * @param Attribute         The name of the attribute (can be 'ID' or 'Description').
 * @param Value             The value of the cell.
 */
:- dynamic symptom/3.

/**
 * clear_symptoms is det.
 *
 * Retract all symptoms/3.
 */
% clear the symptom facts
clear_symptoms :- retractall(symptom(_,_,_)).

/**
 * get_symptoms is semidet.
 * 
 * Fetch and assert all symptom/2 from the database.
 * Requires an open connection named dialysis_connection.
 */
get_symptoms :-
    log_v('database', ['Fetching symptoms...']),
    measure_time,
    odbc_query(dialysis_connection, 
        'SELECT `symptom`.`ID`, `symptom`.`DESCRIPTION` FROM `dialysisai`.`symptom`;', 
        row(ID, Description)),
    assertz(symptom(ID, 'ID', ID)),
    assertz(symptom(ID, 'Description', Description)),
    fail.
get_symptoms :-
    measure_time(Time), count_symptoms(Count), format_ms(Time, TimeString),
    log_i('database', [Count, ' symptoms fetched in ', TimeString, '.']).

/**
 * print_symptoms is det.
 *
 * Print all the symptom facts in the 'ID:Description' format.
 */
print_symptoms :- 
    symptom(ID, 'Description', Description), ID \= 1, println([ID, ':', Description]), fail; true.

/**
 * update_symptoms is semidet.
 *
 * Clear (with clear_symptoms/0) and refresh (with get_symptoms/0) all symptoms.
 */
update_symptoms :- clear_symptoms, get_symptoms.

/**
 * exists_symptom(+ID) is semidet.
 * 
 * Check if the given symptom ID exists.
 *
 * @param ID                ID of the symptom to check for.
 */
exists_symptom(ID) :- symptom(ID, 'ID', ID).

/**
 * exists_symptom(-Count) is det.
 * 
 * Count the number of symptoms.
 *
 * @param Count             The number of existing symptoms.
 */
count_symptoms(Count) :- aggregate_all(count, symptom(ID, 'ID', ID), Count).

% --------------------------------------------------------------------------- %
%                       POSITIVE AND NEGATIVE SYMPTOMS                        %
% --------------------------------------------------------------------------- %

/**
 * ask_target is semidet.
 *
 * Print all existing symptoms and ask the user to choose the one to predict outcomes for.
 * If the user-entered symptom is not valid, the default one is selected, by calling
 * fallback_default_target/0.
 */
ask_target :-
    println('The following is a list of possible symptoms:'),
    print_symptoms,
    println('Please type the symptom ID to start the learning process for.'),
    catch(read(ID), _, (fallback_default_target, fail)),
    (not(number(ID)) -> fallback_default_target; true),
    update_target(ID).

/**
 * fallback_default_target is det.
 * 
 * Fall back to the default target symptom, after printing a warning.
 */
fallback_default_target :-
    log_w('target', 'The target ID you typed is not a number, falling back to the default one.'),
    update_target(2).

/**
 * update_target(+PositiveID) is det.
 *
 * Save the positive and negative symptom IDs to be used for the learning step.
 */
update_target(PositiveID) :-
    retractall(negative_target(_)), retractall(positive_target(_)),
    assertz(negative_target(1)),
    assertz(positive_target(PositiveID)).

% --------------------------------------------------------------------------- %
%                                   RECORDS                                   %
% --------------------------------------------------------------------------- %

/**
 * positive(?ID, ?Attribute, ?Value) is semidet.
 * 
 * Hold positive examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see negative/3
 */
:- dynamic positive/3.

/**
 * negative(?ID, ?Attribute, ?Value) is semidet.
 * 
 * Hold negative examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see positive/3
 */
:- dynamic negative/3.

/**
 * clear_records(+RecordName) is det.
 * 
 * Clear the records from the in-memory Prolog database.
 * 
 * @param RecordName        The name of the record, can be positive or negative.
 */
clear_records(RecordName) :- RecordList =..[RecordName, _, _, _], retractall(RecordList).

/**
 * save_records(+Statement, +RecordName) is det.
 * 
 * Fetch all records from a prepared Statement.
 * 
 * @param Statement         The prepared statement to fetch records from.
 * @param RecordName        The name of the record to assert rows into; can be positive or negative.
 */
save_records(Statement, RecordName) :-
    odbc_fetch(Statement, Row, next), 
    (
        Row == end_of_file ->
            true ;
            Row = row(ID, Patient, Center, PatientSex, PatientRace, PatientAge, SessionID, 
            SessionDate, KTV, QB, ProgWeightLoss, RealWeightLoss, DeltaWeight, ProgDuration, 
            RealDuration, DeltaDuration, SAPStart, SAPEnd, SAPAverage, DAPStart, DAPEnd, 
            DAPAverage, BloodVolume, DeltaBloodFlow, DeltaUF, SymptomID, Score),
            IDList =..[RecordName, ID, 'ID', ID], 
                assertz(IDList),
            PatientList =..[RecordName, ID, 'Patient', Patient], 
                assertz(PatientList),
            CenterList =..[RecordName, ID, 'Center', Center], 
                assertz(CenterList),
            PatientSexList =..[RecordName, ID, 'PatientSex', PatientSex], 
                assertz(PatientSexList),
            PatientRaceList =..[RecordName, ID, 'PatientRace', PatientRace],
                assertz(PatientRaceList),
            PatientAgeList =..[RecordName, ID, 'PatientAge', PatientAge], 
                assertz(PatientAgeList),
            SessionIDList =..[RecordName, ID, 'SessionID', SessionID], 
                assertz(SessionIDList),
            SessionDateList =..[RecordName, ID, 'SessionDate', SessionDate],
                assertz(SessionDateList),
            KTVList =..[RecordName, ID, 'KTV', KTV],
                assertz(KTVList),
            QBList =..[RecordName, ID, 'QB', QB], 
                assertz(QBList),
            ProgWeightLossList =..[RecordName, ID, 'ProgWeightLoss', ProgWeightLoss],
                assertz(ProgWeightLossList),
            RealWeightLossList =..[RecordName, ID, 'RealWeightLoss', RealWeightLoss], 
                assertz(RealWeightLossList),
            DeltaWeightList =..[RecordName, ID, 'DeltaWeight', DeltaWeight], 
                assertz(DeltaWeightList),
            ProgDurationList =..[RecordName, ID, 'ProgDuration', ProgDuration], 
                assertz(ProgDurationList),
            RealDurationList =..[RecordName, ID, 'RealDuration', RealDuration], 
                assertz(RealDurationList),
            DeltaDurationList =..[RecordName, ID, 'DeltaDuration', DeltaDuration], 
                assertz(DeltaDurationList),
            SAPStartList =..[RecordName, ID, 'SAPStart', SAPStart], 
                assertz(SAPStartList),
            SAPEndList =..[RecordName, ID, 'SAPEnd', SAPEnd], 
                assertz(SAPEndList),
            SAPAverageList =..[RecordName, ID, 'SAPAverage', SAPAverage], 
                assertz(SAPAverageList),
            DAPStartList =..[RecordName, ID, 'DAPStart', DAPStart], 
                assertz(DAPStartList),
            DAPEndList =..[RecordName, ID, 'DAPEnd', DAPEnd], 
                assertz(DAPEndList),
            DAPAverageList =..[RecordName, ID, 'DAPAverage', DAPAverage], 
                assertz(DAPAverageList),
            BloodVolumeList =..[RecordName, ID, 'BloodVolume', BloodVolume], 
                assertz(BloodVolumeList),
            DeltaBloodFlowList =..[RecordName, ID, 'DeltaBloodFlow', DeltaBloodFlow], 
                assertz(DeltaBloodFlowList),
            DeltaUFList =..[RecordName, ID, 'DeltaUF', DeltaUF], 
                assertz(DeltaUFList),
            SymptomIDList =..[RecordName, ID, 'SymptomID', SymptomID], 
                assertz(SymptomIDList),
            ScoreList =..[RecordName, ID, 'Score', Score], 
                assertz(ScoreList),
            save_records(Statement, RecordName)
    ).

/**
 * get_records(+SymptomID, +RecordName) is semidet.
 * 
 * Get all records for a given SymptomID and assert them with a given RecordName.
 * 
 * @param SymptomID         The ID of the target attribute of the record.
 * @param RecordName        The name of the record to assert rows into; can be positive or negative.
 */
get_records(SymptomID, RecordName) :-
    measure_time,
    log_v('database', ['Fetching records for symptom ', SymptomID, '...']),
    log_v('database', ['Preparing statement...']),
    % prepare the statement
    odbc_prepare(dialysis_connection, 
        'SELECT `ID`,\c
            `PATIENT`, `CENTER`, `PATIENT_SEX`, `PATIENT_RACE`, `PATIENT_AGE`, `SESSION_ID`, \c
            `SESSION_DATE`, `KTV`, `QB`, `PROG_WEIGHT_LOSS`, `REAL_WEIGHT_LOSS`, `DELTA_WEIGHT`,\c
            `PROG_DURATION`, `REAL_DURATION`, `DELTA_DURATION`, `SAP_START`, `SAP_END`, \c
            `AVG_SAP`, `DAP_START`, `DAP_END`, `AVG_DAP`, `BLOOD_VOLUME`, `DELTA_BLOOD_FLOW`, \c
            `DELTA_UF`, `SYMPTOM_ID`, `SCORE` \c
        FROM `dialysisai`.`patient_dialysis_symptom_for_analysis` \c
        WHERE `SYMPTOM_ID` = ? ORDER BY `SCORE` DESC LIMIT 1000;',
        [default], Statement, [fetch(fetch)]),
    log_v('database', ['Statement prepared.']),
    % execute the statement
    odbc_execute(Statement, [SymptomID], _TheRow),
    % loop over the records and store them
    save_records(Statement, RecordName),
    measure_time(Time), count_records(RecordName, CountRecords), format_ms(Time, TimeString), 
    log_i('database', [CountRecords, ' records fetched in ', TimeString, '.']).

/**
 * print_records(RecordName) is det.
 * 
 * Print all records with the given RecordName
 * 
 * @param RecordName        The name of the record to assert rows into; can be positive or negative.
 */
print_records(RecordName) :-
    _RecordList =..[RecordName, ID, Attribute, Value], 
    println([ID, '.', Attribute, ' = ', Value]), fail;
    true.

/**
 * update_records(+SymptomID) is semidet.
 *
 * Update both positive and negative examples by fetching them from the database.
 * The negative ID is fixed (1), but the positive can be passed to this predicate.
 *
 * @param SymptomID         The ID of the symptom to be used as positive target, or:
 *                            - default, use the 2
 *                            - ask, let the user decide
 */
% update records with default positive (2)
update_records(default) :-
    % use the default target symptom ID (2)
    update_target(2),
    % fallthrough
    fail.
% update records by asking the user for the positive
update_records(ask) :-
    % ask for the positive symptom ID
    ask_target,
    % fallthrough
    fail.
% update records with a given positive ID
update_records(SymptomID) :-
    % if the target symptom ID is a number
    number(SymptomID),
    % use it
    update_target(SymptomID),
    % fallthrough
    fail.
% always update in the end
update_records(_) :- 
    % update the records
    update_records.

/**
 * update_records is semidet.
 * 
 * Clear (clear_records/1) and updates (get_records/2) all positive and negative examples.
 */
% 
update_records :-
    clear_records(negative),            % clear negative records
    clear_records(positive),            % clear positive records
    negative_target(NegNumb), atom_number(Neg, NegNumb),
    get_records(Neg, negative),     % get negative records
    positive_target(PosNumb), atom_number(Pos, PosNumb),
    get_records(Pos, positive).     % get positive records

/**
 * exists_record(?RecordName, ?ID) is semidet.
 * 
 * Check if the given record ID was asserted with the given RecordName.
 * 
 * @param RecordName        The name the record was asserted with; can be positive or negative.
 * @param ID                The ID of the record.
 */
exists_record(RecordName, ID) :-
    RecordList =..[RecordName, ID, 'ID', ID], RecordList.

/**
 * count_records(?RecordName, ?Count) is det.
 * 
 * Count the number of existing records with a given name.
 * 
 * @param RecordName        The name of the record to count; can be positive or negative.
 * @param Count             The number of records.
 */
% count the number of records
count_records(RecordName, Count) :-
    RecordList =..[RecordName, ID, 'ID', ID], aggregate_all(count, RecordList, Count).

% --------------------------------------------------------------------------- %
%                              ACCESSORY METHODS                              %
% --------------------------------------------------------------------------- %

/**
 * example(Kind, ID, Attribute, Value) is semidet.
 * 
 * Accessory predicate to access any kind of example by specifying the name the record was
 * asserted with (negative or positive).
 * 
 * @param Kind              The name the record was asserted with; can be positive or negative.
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute of the example.
 * @param Value             The Value of the example.
 */
example(positive, ID, Attribute, Value) :- positive(ID, Attribute, Value).
example(negative, ID, Attribute, Value) :- negative(ID, Attribute, Value).