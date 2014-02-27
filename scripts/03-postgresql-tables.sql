﻿CREATE TABLE "PATIENT_DIALYSIS_SYMPTOM"
(
	"ID" bigint NOT NULL, 
	"PATIENT" bigint NULL,
	"CENTER" bigint NULL,
	"PATIENT_SEX" bigint NULL,
	"PATIENT_RACE" bigint NULL,
	"PATIENT_AGE" bigint NULL,
	"SESSION_ID" bigint NULL,
	"SESSION_DATE" timestamp with time zone NULL,
	"KTV" float NULL,
	"QB" float NULL,
	"REAL_WEIGHT_LOSS" float NULL,
	"PROG_WEIGHT_LOSS" float NULL,
	"DELTA_WEIGHT" float NULL,
	"PROG_DURATION" float NULL,
	"REAL_DURATION" float NULL,
	"DELTA_DURATION" float NULL,
	"SAP_START" float NULL,
	"SAP_END" float NULL,
	"AVG_SAP" float NULL,
	"DAP_START" float NULL,
	"DAP_END" float NULL,
	"AVG_DAP" float NULL,
	"BLOOD_VOLUME" float NULL,
	"DELTA_BLOOD_FLOW" float NULL,
	"DELTA_UF" float NULL,
	"SYMPTOM_ID" bigint NULL,
	CONSTRAINT "PK_PATIENT_DIALYSIS_SYMPTOM" PRIMARY KEY ("ID")
) WITH ( OIDS = FALSE );

CREATE TABLE "RACE"(
	"ID" int NOT NULL,
	"DESCRIPTION" varchar(50) NULL,
	CONSTRAINT "PK_RACE" PRIMARY KEY("ID")
) WITH ( OIDS = FALSE );

CREATE TABLE "SYMPTOM"(
	"ID" int NOT NULL,
	"DESCRIPTION" varchar(50) NULL,
	CONSTRAINT "PK_SYMPTOM" PRIMARY KEY("ID")
) WITH ( OIDS = FALSE );