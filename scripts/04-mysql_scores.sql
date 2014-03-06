-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_SCORE                            --
-- ----------------------------------------------------------------------------
-- The view is needed to calculate the number of non-null columns on a total --
-- of 13. The higher the number of columns, the higher the score. 			 --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE` AS
  SELECT
  `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.*,
  (
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`KTV`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`QB`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`PROG_WEIGHT_LOSS`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`REAL_WEIGHT_LOSS`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`PROG_DURATION`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`REAL_DURATION`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`SAP_START`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`SAP_END`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`AVG_SAP`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_START`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_END`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`AVG_DAP`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`BLOOD_VOLUME`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_BLOOD_FLOW`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_UF`),0,1)
  ) AS `SCORE` 
  FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_FOR_ANALYSIS                                      --
-- ----------------------------------------------------------------------------
-- This view retrieves the patient with enough good data for analysis.       --
-- To set a custom non-null percentage threshold, do the following:          --
-- * let percentage be the desired non-null percentage                       --
-- * let columns be the number of columns to evaluate (currently 15)         --
-- * the score must be equal or higher than columns*percentage/100           --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_FOR_ANALYSIS` AS
SELECT PATIENT, CENTER, COUNT(*) AS "COUNT"
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE`
-- get only rows with >= 80% of non-null columns
WHERE (`SCORE` >= (15*80/100))
GROUP BY PATIENT, CENTER
-- get only patients with at least 5 "good" rows
HAVING (COUNT(*) >= 5);

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE                       --
-- ----------------------------------------------------------------------------
-- This view retrieves the "acceptable" rows, with a score equal or higher   --
-- than 80%.                                                                 --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE` AS
SELECT *
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE`
WHERE
	(`PATIENT`,`CENTER`) IN (SELECT `PATIENT`, `CENTER` FROM `DialysisAI`.`PATIENT_FOR_ANALYSIS`)
AND (`SCORE` >= (15*80/100));

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_AVGS
-- ----------------------------------------------------------------------------
-- This view calculates the average values (avoiding null) for each patient  --
-- so that they can be used to replace null occurrences during analysis.     --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS` AS
SELECT
	`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT`,
	`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`CENTER`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`KTV`) AS `AVG_KTV`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`QB`) AS `AVG_QB`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_WEIGHT_LOSS`) AS `AVG_PROG_WEIGHT_LOSS`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_WEIGHT_LOSS`) AS `AVG_REAL_WEIGHT_LOSS`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_WEIGHT`) AS `AVG_DELTA_WEIGHT`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_DURATION`) AS `AVG_PROG_DURATION`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_DURATION`) AS `AVG_REAL_DURATION`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_DURATION`) AS `AVG_DELTA_DURATION`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_START`) AS `AVG_SAP_START`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_END`) AS `AVG_SAP_END`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_SAP`) AS `AVG_AVG_SAP`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_START`) AS `AVG_DAP_START`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_END`) AS `AVG_DAP_END`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_dAP`) AS `AVG_AVG_DAP`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`BLOOD_VOLUME`) AS `AVG_BLOOD_VOLUME`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_BLOOD_FLOW`) AS `AVG_DELTA_BLOOD_FLOW`,
	AVG(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_UF`) AS `AVG_DELTA_UF`
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`
GROUP BY `PATIENT`, `CENTER`;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_FOR_ANALYSIS                     --
-- ----------------------------------------------------------------------------
-- This view will be used for the real data analysis, as every row will      --
-- contain non null values, using the averages if needed.                    --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_FOR_ANALYSIS` AS
SELECT
	`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`ID`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`CENTER`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT_SEX`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT_RACE`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT_AGE`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SESSION_ID`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SESSION_DATE`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`KTV`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_KTV`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`KTV`) AS `KTV`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`QB`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_QB`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`QB`) AS `QB`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_WEIGHT_LOSS`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_PROG_WEIGHT_LOSS`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_WEIGHT_LOSS`) AS `PROG_WEIGHT_LOSS`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_WEIGHT_LOSS`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_REAL_WEIGHT_LOSS`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_WEIGHT_LOSS`) AS `REAL_WEIGHT_LOSS`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_WEIGHT`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DELTA_WEIGHT`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_WEIGHT`) AS `DELTA_WEIGHT`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_DURATION`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_PROG_DURATION`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PROG_DURATION`) AS `PROG_DURATION`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_DURATION`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_REAL_DURATION`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`REAL_DURATION`) AS `REAL_DURATION`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_DURATION`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DELTA_DURATION`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_DURATION`) AS `DELTA_DURATION`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_START`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_SAP_START`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_START`) AS `SAP_START`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_END`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_SAP_END`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SAP_END`) AS `SAP_END`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_SAP`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_AVG_SAP`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_SAP`) AS `AVG_SAP`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_START`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DAP_START`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_START`) AS `DAP_START`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_END`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DAP_END`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DAP_END`) AS `DAP_END`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_DAP`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_AVG_DAP`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`AVG_DAP`) AS `AVG_DAP`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`BLOOD_VOLUME`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_BLOOD_VOLUME`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`BLOOD_VOLUME`) AS `BLOOD_VOLUME`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_BLOOD_FLOW`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DELTA_BLOOD_FLOW`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_BLOOD_FLOW`) AS `DELTA_BLOOD_FLOW`,
    IF(ISNULL(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_UF`),
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`AVG_DELTA_UF`,
		`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`DELTA_UF`) AS `DELTA_UF`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SYMPTOM_ID`,
    `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`SCORE`
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`
INNER JOIN `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`
ON `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`PATIENT` = `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`PATIENT`
AND `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_ACCEPTABLE`.`CENTER` = `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_AVGS`.`CENTER`;
