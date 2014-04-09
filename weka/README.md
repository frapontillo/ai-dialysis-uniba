Weka
========

## Step 1: data selection

### From MySQL

#### Setup
In order to connect from Weka to the local MySQL instance, edit the last line of

    {WEKA_PATH}/RunWeka.ini

adding the MySQL JDBC connector path to the classpath, e.g.:

    cp=%CLASSPATH%;C:/Program Files (x86)/MySQL/Connector J 5.1.29/mysql-connector-java-5.1.29-bin.jar

#### Connection
Open the database with the following parameters:
	* URL: `jdbc:mysql://localhost:3306/DialysisAI`
	* Username: `root`
	* Password: `mysql`

#### Querying
Execute the following query:

```sql
(SELECT 
`PATIENT_SEX`, `PATIENT_RACE`, `PATIENT_AGE`,  `KTV`, `QB`, `PROG_WEIGHT_LOSS`, `REAL_WEIGHT_LOSS`, `DELTA_WEIGHT`, `PROG_DURATION`, `REAL_DURATION`, `DELTA_DURATION`, `SAP_START`, `SAP_END`, `AVG_SAP`, `DAP_START`, `DAP_END`, `AVG_DAP`, `BLOOD_VOLUME`, `DELTA_BLOOD_FLOW`, `DELTA_UF`, `SYMPTOM_ID`
FROM `dialysisai`.`patient_dialysis_symptom_for_analysis`
WHERE `SYMPTOM_ID` = 1 ORDER BY `SCORE` DESC LIMIT 100)
UNION
(SELECT
`PATIENT_SEX`, `PATIENT_RACE`, `PATIENT_AGE`, `KTV`, `QB`, `PROG_WEIGHT_LOSS`, `REAL_WEIGHT_LOSS`, `DELTA_WEIGHT`, `PROG_DURATION`, `REAL_DURATION`, `DELTA_DURATION`, `SAP_START`, `SAP_END`, `AVG_SAP`, `DAP_START`, `DAP_END`, `AVG_DAP`, `BLOOD_VOLUME`, `DELTA_BLOOD_FLOW`, `DELTA_UF`, `SYMPTOM_ID`
FROM `dialysisai`.`patient_dialysis_symptom_for_analysis`
WHERE `SYMPTOM_ID` = 8 ORDER BY `SCORE` DESC LIMIT 100)
```

#### Data transformation
Some numeric attributes must be converted into nominal first. Select the following attributes:
	* `PATIENT_SEX`
	* `PATIENT_RACE`
	* `SYMPTOM_ID`
Then, select the `NumericToNominal` filter and apply it with the default `-R first-last` option, so it will convert all instances of those variables.

### From file

You can select the provided already-filtered ARFF file.

## Step 2: running the algorithms

Select the **Classify** tab and one of the following algorithms:
    * `trees.J48`, then load the `trees.J48.params` parameters file
    * `trees.J48graft`, then load the `trees.J48graft.params` parameters file
    * `rules.ConjunctiveRule`, then load the `rules.ConjunctiveRule.params` parameters file

