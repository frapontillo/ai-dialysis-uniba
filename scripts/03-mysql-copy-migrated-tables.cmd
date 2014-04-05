REM Workbench Table Data copy script
REM 
REM Execute this to copy table data from a source RDBMS to MySQL.
REM Edit the options below to customize it. You will need to provide passwords, at least.
REM 
REM Source DB: Mssql@DRIVER=SQL Server;SERVER=DELL (Microsoft SQL Server)
REM Target DB: Mysql@localhost:3306


@ECHO OFF
REM Source and target DB passwords
FOR /F "tokens=1* delims==" %%G IN (connection.properties) DO (set %%G=%%H)

IF [%arg_source_password%] == [] (
    IF [%arg_target_password%] == [] (
        ECHO WARNING: Both source and target RDBMSes passwords are empty. You should edit the connection.properties file to set them.
    )
)
set arg_worker_count=2
REM Uncomment the following options according to your needs

REM Whether target tables should be truncated before copy
REM set arg_truncate_target=--truncate-target
REM Enable debugging output
set arg_debug_output=--log-level=debug3


REM Creation of file with table definitions for copytable

set table_file="%TMP%\wb_tables_to_migrate.txt"
TYPE NUL > "%TMP%\wb_tables_to_migrate.txt"
ECHO [DialysisAI]	[dbo].[SYMPTOM]	`DialysisAI`	`SYMPTOM`	[ID], CAST([DESCRIPTION] as NVARCHAR(70)) as [DESCRIPTION] >> "%TMP%\wb_tables_to_migrate.txt"
ECHO [DialysisAI]	[dbo].[PATIENT_DIALYSIS_SYMPTOM]	`DialysisAI`	`PATIENT_DIALYSIS_SYMPTOM`	[ID], [PATIENT], [CENTER], [PATIENT_SEX], [PATIENT_RACE], [PATIENT_AGE], [SESSION_ID], [SESSION_DATE], [KTV], [QB], [PROG_WEIGHT_LOSS], [REAL_WEIGHT_LOSS], [DELTA_WEIGHT], [PROG_DURATION], [REAL_DURATION], [DELTA_DURATION], [SAP_START], [SAP_END], [AVG_SAP], [DAP_START], [DAP_END], [AVG_DAP], [BLOOD_VOLUME], [DELTA_BLOOD_FLOW], [DELTA_UF], [SYMPTOM_ID] >> "%TMP%\wb_tables_to_migrate.txt"
ECHO [DialysisAI]	[dbo].[RACE]	`DialysisAI`	`RACE`	[ID], CAST([DESCRIPTION] as NVARCHAR(50)) as [DESCRIPTION] >> "%TMP%\wb_tables_to_migrate.txt"


wbcopytables.exe --odbc-source="DRIVER={SQL Server};SERVER=DELL;DATABASE={};UID=sa" --target="root@localhost:3306" --source-password="%arg_source_password%" --target-password="%arg_target_password%" --table-file="%table_file%" --thread-count=%arg_worker_count% %arg_truncate_target% %arg_debug_output%

REM Removes the file with the table definitions
DEL "%TMP%\wb_tables_to_migrate.txt"

pause
