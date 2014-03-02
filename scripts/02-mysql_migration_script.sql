-- ----------------------------------------------------------------------------
-- MySQL Workbench Migration
-- Migrated Schemata: DialysisAI
-- Source Schemata: DialysisAI
-- Created: Sun Mar 02 11:15:44 2014
-- ----------------------------------------------------------------------------

SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------------------------------------------------------
-- Schema DialysisAI
-- ----------------------------------------------------------------------------
DROP SCHEMA IF EXISTS `DialysisAI` ;
CREATE SCHEMA IF NOT EXISTS `DialysisAI` ;

-- ----------------------------------------------------------------------------
-- Table DialysisAI.RACE
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `DialysisAI`.`RACE` (
  `ID` INT NOT NULL,
  `DESCRIPTION` VARCHAR(50) NULL,
  PRIMARY KEY (`ID`));

-- ----------------------------------------------------------------------------
-- Table DialysisAI.SYMPTOM
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `DialysisAI`.`SYMPTOM` (
  `ID` INT NOT NULL,
  `DESCRIPTION` VARCHAR(70) NULL,
  PRIMARY KEY (`ID`));

-- ----------------------------------------------------------------------------
-- Table DialysisAI.PATIENT_DIALYSIS_SYMPTOM
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM` (
  `ID` BIGINT NOT NULL AUTO_INCREMENT,
  `PATIENT` BIGINT NULL,
  `CENTER` BIGINT NULL,
  `PATIENT_SEX` BIGINT NULL,
  `PATIENT_RACE` BIGINT NULL,
  `PATIENT_AGE` BIGINT NULL,
  `SESSION_ID` BIGINT NULL,
  `SESSION_DATE` DATETIME NULL,
  `KTV` DOUBLE NULL,
  `QB` DOUBLE NULL,
  `PROG_WEIGHT_LOSS` DOUBLE NULL,
  `REAL_WEIGHT_LOSS` DOUBLE NULL,
  `DELTA_WEIGHT` DOUBLE NULL,
  `PROG_DURATION` DOUBLE NULL,
  `REAL_DURATION` DOUBLE NULL,
  `DELTA_DURATION` DOUBLE NULL,
  `SAP_START` DOUBLE NULL,
  `SAP_END` DOUBLE NULL,
  `AVG_SAP` DOUBLE NULL,
  `DAP_START` DOUBLE NULL,
  `DAP_END` DOUBLE NULL,
  `AVG_DAP` DOUBLE NULL,
  `BLOOD_VOLUME` DOUBLE NULL,
  `DELTA_BLOOD_FLOW` DOUBLE NULL,
  `DELTA_UF` DOUBLE NULL,
  `SYMPTOM_ID` BIGINT NULL,
  PRIMARY KEY (`ID`));
SET FOREIGN_KEY_CHECKS = 1;;

-- ----------------------------------------------------------------------------
-- View DialysisAI.PATIENT_DIALYSIS_SYMPTOM_SCORE
-- ----------------------------------------------------------------------------
CREATE OR REPLACE VIEW `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM_SCORE` AS
  select
  `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.*,
  (
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`KTV`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`QB`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`PROG_WEIGHT_LOSS`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`REAL_WEIGHT_LOSS`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`PROG_DURATION`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`REAL_DURATION`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`SAP_START`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`SAP_END`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_START`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DAP_END`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`BLOOD_VOLUME`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_BLOOD_FLOW`),1,0) + 
    if(isnull(`DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`.`DELTA_UF`),1,0)
  ) AS `SCORE` 
  from `DialysisAI`.`PATIENT_DIALYSIS_SYMPTOM`;