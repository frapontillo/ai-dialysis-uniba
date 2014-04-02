:- ['database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl'].
:- use_module(library('dialect/hprolog')).

:- println('Welcome!').

main(default) :- main('config/database.properties').

main(Config) :-
    connect(Config),                % connect to the database
    after_connection.

main :-
    connect,
    after_connection.

after_connection :-
    update_symptoms,                % update all of the possible symptoms
    update_records,                 % get positive and negative examples
    update_categories               % update the categories based on the retrieved examples
    .