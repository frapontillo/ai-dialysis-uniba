Running scripts
===============

## Prerequisites

The original dialysis database must be "SourceDB" and should be running on a SQL Server instance.

The target data will be stored in a MySQL instance, with the `wbcopytables` (installed by MySQL Workbench) utility binary in the environment path.

## Step 1: data normalization

The first step consists in normalizing the original tables data into some more manageable information.

- `RACE` contains all of the patient races
- `SYMPTOM` contains all of the dialysis symptoms
- `PATIENT_DIALYSIS_SYMPTOM` contains the complete list of
	- **patient** information
		- `PATIENT`, the patient ID
		- `CENTER`, the center where the dialysis was executed
		- `PATIENT_SEX`, the sex
		- `PATIENT_RACE`, the race
		- `PATIENT_AGE`, the age on the dialysis date
	- **dialysis** data
		- `SESSION_ID`, the ID of the dialysis session
		- `SESSION_DATE`, the date of the dialysis execution
		- `KTV`, the KTV value
		- `QB`, the QB value
		- `PROG_WEIGHT_LOSS`, the programmed weight loss
		- `REAL_WEIGHT_LOSS`, the actual weight loss
		- `DELTA_WEIGHT`, the weight difference
		- `PROG_DURATION`, the programmed duration of the dialysis
		- `REAL_DURATION`, the actual duration of the dialysis
		- `DELTA_DURATION`, the difference between the actual duration and the programmed one
		- `SAP_START`, the systolic arterial pressure before the dialysis
		- `SAP_END`, the systolic arterial pressure after the dialysis
		- `AVG_SAP`, the average systolic arterial pressure
		- `DAP_START`, the diastolic arterial pressure before the dialysis
		- `DAP_END`, the diastolic arterial pressure after the dialysis
		- `AVG_DAP`, the average distolic arterial pressure
		- `BLOOD_VOLUME`, the processed blood volume
		- `DELTA_BLOOD_FLOW`, the difference of blood flow
		- `DELTA_UF`, the difference of ultrafiltration
	- target **symptom**, `SYMPTOM_ID`

The queries can be executed launching `scripts/01-sql-server-tables.sql` on the SQL Server instance.

## Step 2: MySQL database creation

Before being able to import the data into the MySQL instance, the same database structure must be replicated onto the MySQL server.

Execute `scripts/02-mysql-migration-script.sql` to create the database and the tables.

## Step 3: data import

To transfer all of the data from SQL Server to MySQL, do the following:

1. Create the `scripts/connection.properties` file (see `scripts/connection.properties.example` for an example) .
2. Start the `scripts/03-mysql-copy-migrated-tables.cmd` to migrate all of the data.
3. The `dialysisai` schema will be created and populated on the local machine.

## Step 4: custom view creation

The analysis isn't executed on the imported data, as the information has to be normalized and filtered before a valid learning process can be started. To create the views that take care of this process, launch the `scripts/04-mysql-scores.sql` script.

## Step 5: to the clouds!

You can export the database from a local instance to a cloud-based one using the MySQL Export/Import Data tool that will generate the necessary scripts:

1. Export the tables only.
2. Import the tables.
3. Execute the `scripts/04-mysql-scores.sql` script on the new database.