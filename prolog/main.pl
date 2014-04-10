/** <module> Main module

This module initializes all needed configurations, facts and rules 
by calling other modules' predicates.

@author Francesco Pontillo
@license Apache License, Version 2.0
*/

:- ['database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl', library('dialect/hprolog')].

:- println('Welcome!').

/**
 * main(+Config, +SymptomID) is det.
 * 
 * Start the main process with custom parameters.
 *
 * @param Config            The name of the configuration file. It can be:
 *                            - default, for the default 'database.properties' file
 *                            - ask, in order to type in the file name
 *                            - a proper configuration file name
 * @param Symptom           The symptom ID to use as positive target. It can be:
 *                            - default, for the default (2)
 *                            - ask, in order to type in the ID
 *                            - a proper ID
 */
main(Config, Symptom) :-
    connect(Config),                            % connect to the database
    update_symptoms,                            % update all of the possible symptoms
    update_records(Symptom),                    % get positive and negative examples
    update_categories,                          % update categories based on retrieved examples
    learn_please,                               % learn!
    true.

/**
 * main_def is det.
 *
 * Start main/2 with default parameters.
 */
main_def :-
    main(default, default).

/**
 * main is det.
 *
 * Start main/2 and asks for configurations.
 */
main :-
    main(ask, ask).

/**
 * make_doc is det.
 * 
 * Generate the documentation in both html and tex formats.
 * Both documentations will be under the 'doc' folder.
 */
make_doc :-
	doc_save(., [doc_root('doc'), title('AI Dialyisis Symptomatology')]),
	doc_latex(
		['main.pl', 'database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl'], 
		'doc/doc.tex',
		[stand_alone(false)]), !, fail;
    doc_latex(
        ['main.pl', 'database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl'], 
        'doc/doc_full.tex',
        []), !
	.

/**
 * out is det.
 * 
 * Disconnects and halts.
 */
 out :- disconnect, halt.