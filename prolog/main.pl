:- ['database.pl'].

main :- connect, session_db(row(ID, Patient, Center, Sex, Race, Age)), write(row(ID, Patient, Center, Sex, Race, Age)), nl, disconnect.