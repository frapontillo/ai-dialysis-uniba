-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_SCORE
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
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_START`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_END`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`BLOOD_VOLUME`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_BLOOD_FLOW`),0,1) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_UF`),0,1)
  ) AS `SCORE` 
  FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_FOR_ANALYSIS
-- ----------------------------------------------------------------------------
-- To set a custom non-null percentage threshold, do the following:
-- - let percentage be the desired non-null percentage
-- - let columns be the number of columns to evaluate (currently 13)
-- - the score must be equal or higher than columns*percentage/100

CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_FOR_ANALYSIS` AS
SELECT PATIENT, CENTER, COUNT(*) AS "COUNT"
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE`
-- get only rows with >= 80% of non-null columns
WHERE (`SCORE` >= (13*80/100))
GROUP BY PATIENT, CENTER
-- get only patients with at least 5 "good" rows
HAVING (COUNT(*) >= 5);

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_FOR_ANALYSIS
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_FOR_ANALYSIS` AS
SELECT *
FROM `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE`
WHERE
	(`PATIENT`,`CENTER`) IN (SELECT `PATIENT`, `CENTER` FROM `DialysisAI`.`PATIENT_FOR_ANALYSIS`)
AND (`SCORE` >= (13*80/100));