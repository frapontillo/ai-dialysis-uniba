from bayesdb.client import Client
client = Client()
client('DROP BTABLE dialysisai;')
client('CREATE BTABLE dialysisai FROM learn_data.csv;')
client('UPDATE DATATYPES FROM dialysisai SET PROG_DURATION=continuous, BLOOD_VOLUME=continuous, REAL_SYMPTOM_ID=ignore;')
client('CREATE 20 MODELS FOR dialysisai;')
client('ANALYZE dialysisai FOR 100 ITERATIONS;')