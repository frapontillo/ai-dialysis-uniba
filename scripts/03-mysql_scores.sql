SET @score_percentage = 85;
SET @min_score = (13*@score_percentage/100);

SELECT PATIENT, CENTER, COUNT(*) AS "COUNT"
FROM dialysisai.patient_dialysis_symptom_score
WHERE (`score` >= @min_score)
GROUP BY PATIENT, CENTER
HAVING (COUNT(*) >= 5);