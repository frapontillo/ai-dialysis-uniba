-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_SCORE                            --
-- ----------------------------------------------------------------------------
-- The view is needed to calculate the number of non-null columns on a total --
-- of 13. The higher the number of columns, the higher the score. 			 --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `dialysisai`.`patient_dialysis_symptom_score` AS
  SELECT
  `dialysisai`.`patient_dialysis_symptom`.*,
  (
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`KTV`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`QB`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`PROG_WEIGHT_LOSS`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`REAL_WEIGHT_LOSS`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`PROG_DURATION`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`REAL_DURATION`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`SAP_START`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`SAP_END`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`AVG_SAP`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`DAP_START`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`DAP_END`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`AVG_DAP`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`BLOOD_VOLUME`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`DELTA_BLOOD_FLOW`),0,1) + 
    if(isnull(`dialysisai`.`patient_dialysis_symptom`.`DELTA_UF`),0,1)
  ) AS `SCORE` 
  FROM `dialysisai`.`patient_dialysis_symptom`;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_FOR_ANALYSIS                                      --
-- ----------------------------------------------------------------------------
-- This view retrieves the patient with enough good data for analysis.       --
-- To set a custom non-null percentage threshold, do the following:          --
-- * let percentage be the desired non-null percentage                       --
-- * let columns be the number of columns to evaluate (currently 15)         --
-- * the score must be equal or higher than columns*percentage/100           --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `dialysisai`.`patient_for_analysis` AS
	SELECT PATIENT, CENTER, COUNT(*) AS "COUNT"
	FROM `dialysisai`.`patient_dialysis_symptom_score`
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
CREATE OR REPLACE VIEW `dialysisai`.`patient_dialysis_symptom_acceptable` AS
	SELECT *
	FROM `dialysisai`.`patient_dialysis_symptom_score`
	WHERE
		(`PATIENT`,`CENTER`) IN (SELECT `PATIENT`, `CENTER` FROM `dialysisai`.`patient_for_analysis`)
	AND (`SCORE` >= (15*80/100));

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_AVGS
-- ----------------------------------------------------------------------------
-- This view calculates the average values (avoiding null) for each patient  --
-- so that they can be used to replace null occurrences during analysis.     --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `dialysisai`.`patient_dialysis_symptom_avgs` AS
SELECT
	`dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT`,
	`dialysisai`.`patient_dialysis_symptom_acceptable`.`CENTER`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`KTV`) AS `AVG_KTV`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`QB`) AS `AVG_QB`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_WEIGHT_LOSS`) AS `AVG_PROG_WEIGHT_LOSS`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_WEIGHT_LOSS`) AS `AVG_REAL_WEIGHT_LOSS`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_WEIGHT`) AS `AVG_DELTA_WEIGHT`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_DURATION`) AS `AVG_PROG_DURATION`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_DURATION`) AS `AVG_REAL_DURATION`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_DURATION`) AS `AVG_DELTA_DURATION`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_START`) AS `AVG_SAP_START`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_END`) AS `AVG_SAP_END`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_SAP`) AS `AVG_AVG_SAP`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_START`) AS `AVG_DAP_START`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_END`) AS `AVG_DAP_END`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_dAP`) AS `AVG_AVG_DAP`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`BLOOD_VOLUME`) AS `AVG_BLOOD_VOLUME`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_BLOOD_FLOW`) AS `AVG_DELTA_BLOOD_FLOW`,
	AVG(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_UF`) AS `AVG_DELTA_UF`
FROM `dialysisai`.`patient_dialysis_symptom_acceptable`
GROUP BY `PATIENT`, `CENTER`;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_FOR_ANALYSIS                     --
-- ----------------------------------------------------------------------------
-- This view will be used for the real data analysis, as every row will      --
-- contain non null values, using the averages if needed.                    --
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `dialysisai`.`patient_dialysis_symptom_for_analysis` AS
SELECT
	`dialysisai`.`patient_dialysis_symptom_acceptable`.`ID`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`CENTER`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT_SEX`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT_RACE`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT_AGE`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`SESSION_ID`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`SESSION_DATE`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`KTV`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_KTV`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`KTV`) AS `KTV`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`QB`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_QB`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`QB`) AS `QB`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_WEIGHT_LOSS`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_PROG_WEIGHT_LOSS`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_WEIGHT_LOSS`) AS `PROG_WEIGHT_LOSS`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_WEIGHT_LOSS`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_REAL_WEIGHT_LOSS`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_WEIGHT_LOSS`) AS `REAL_WEIGHT_LOSS`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_WEIGHT`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DELTA_WEIGHT`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_WEIGHT`) AS `DELTA_WEIGHT`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_DURATION`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_PROG_DURATION`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`PROG_DURATION`) AS `PROG_DURATION`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_DURATION`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_REAL_DURATION`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`REAL_DURATION`) AS `REAL_DURATION`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_DURATION`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DELTA_DURATION`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_DURATION`) AS `DELTA_DURATION`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_START`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_SAP_START`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_START`) AS `SAP_START`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_END`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_SAP_END`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`SAP_END`) AS `SAP_END`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_SAP`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_AVG_SAP`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_SAP`) AS `AVG_SAP`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_START`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DAP_START`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_START`) AS `DAP_START`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_END`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DAP_END`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DAP_END`) AS `DAP_END`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_DAP`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_AVG_DAP`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`AVG_DAP`) AS `AVG_DAP`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`BLOOD_VOLUME`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_BLOOD_VOLUME`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`BLOOD_VOLUME`) AS `BLOOD_VOLUME`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_BLOOD_FLOW`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DELTA_BLOOD_FLOW`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_BLOOD_FLOW`) AS `DELTA_BLOOD_FLOW`,
    IF(ISNULL(`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_UF`),
		`dialysisai`.`patient_dialysis_symptom_avgs`.`AVG_DELTA_UF`,
		`dialysisai`.`patient_dialysis_symptom_acceptable`.`DELTA_UF`) AS `DELTA_UF`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`SYMPTOM_ID`,
    `dialysisai`.`patient_dialysis_symptom_acceptable`.`SCORE`
FROM `dialysisai`.`patient_dialysis_symptom_acceptable`
INNER JOIN `dialysisai`.`patient_dialysis_symptom_avgs`
ON `dialysisai`.`patient_dialysis_symptom_acceptable`.`PATIENT` = `dialysisai`.`patient_dialysis_symptom_avgs`.`PATIENT`
AND `dialysisai`.`patient_dialysis_symptom_acceptable`.`CENTER` = `dialysisai`.`patient_dialysis_symptom_avgs`.`CENTER`;
