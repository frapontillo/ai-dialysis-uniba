/** <module> main
 *
 *  This module initializes all needed configuratins, facts and rules 
 *  by calling other modules' predicates.
 *
 *  @author Francesco Pontillo
 *  @license Apache License, Version 2.0
*/

:- ['database.pl', 'categories.pl', 'util.pl', 'learner.pl', 'log.pl'].
:- use_module(library('dialect/hprolog')).

:- println('Welcome!').

/**
 * TODO: write doc
 */
main(Config) :-
    (
        Config = default ->                         % if the configuration is the default one
        Path = 'config/database.properties';        % use the default file
        Path = Config                               % otherwise use the provided one
    ),
    connect(Path),                                  % connect to the database
    after_connection(Config).                       % do other stuff

/**
 * TODO: write doc
 */
main :-
    connect,                                        % connect to the database
    after_connection(not_default).                  % do other stuff

/**
 * TODO: write doc
 */
after_connection(Default) :-
    update_symptoms,                                % update all of the possible symptoms
    (
        Default = default ->                        % update the target (positive) ID to learn for
        default_target;
        update_target
    ),
    update_records,                                 % get positive and negative examples
    update_categories,                              % update the categories based on the retrieved examples
    %learn_please,                                   % learn!
    true
    .
