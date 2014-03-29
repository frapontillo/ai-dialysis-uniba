:- ['database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl'].
:- use_module(library('dialect/hprolog')).

main :-
	println('Welcome!'),
	connect,								% connect to the database
	update_symptoms,				% update all of the possible symptoms
	update_records,					% get positive and negative examples
	update_categories				% update the categories based on the retrieved examples
	.