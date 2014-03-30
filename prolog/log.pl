% ---------------- %
%        LOG       %
% ---------------- %

% define logging levels
log_level(2, verbose, ' V ', [bold,bg(default)]).
log_level(3, debug, ' D ', [bold,bg(cyan)]).
log_level(4, info, ' I ', [bold,bg(green)]).
log_level(5, warn, ' W ', [bold,bg(yellow)]).
log_level(6, error, ' E ', [bold,bg(red)]).
log_level(7, assert, ' A ', [bold,bg(magenta)]).

/**
 * Log a message at a certain level with a given tag.
 * 
 * @param Level the level to log the message with, choose between
 *              verbose, debug, info, warn, error, assert.
 * @param Tag   the tag to use as the log header.
 * @param Log   the actual log message.
 */
log_at(Level, Tag, Log) :-
    can_log(Level),
    % log the tag
    log_tag(Level, Tag, 13),
    % get the level
    log_level(_, Level, LevelText, Format),
    % log the level type
    write(' '),
    ansi_format(Format, LevelText, []),
    write(' '),
    % log the actual message
    println(Log), !.

% always succeed (even if the log cannot be printed)
log_at(_, _, _).

% shortcut method to avoid the logging of the tag
log_at(Level, Log) :-
    log_at(Level, '', Log).

/**
 * Log a tag with a certain level (used for the color) and with 
 * a character limit, beyond which ellipsis are added.
 * 
 * @param Level the level to log the tag with, choose between
 *              verbose, debug, info, warn, error, assert.
 * @param Tag   the tag to print.
 * @param Max   the maximum tag length.
 */
log_tag(Level, Tag, Max) :-
    % get the level
    log_level(_, Level, _, Format),
    % get the color
    member(bg(Color), Format),

    % shorten the tag, if necessary
    atom_chars(Tag, TagList),
    (
        % if text is too long
        length(TagList, TagLength), TagLength > Max ->
            % then split it
            split_at(Max-3, TagList, NewTagList, _),
            Ellipsis = '...';
            % else use that
            NewTagList = TagList,
            Ellipsis = ''
    ),
    % write the tag as a string
    atomic_list_concat(NewTagList, NewTag),
    ansi_format([bold, fg(Color)], NewTag, []),
    % write an empty character for each remaining letter, if any
    (
        string_length(NewTag, NewTagLength),
        string_length(Ellipsis, EllipsisLength),
        Spaces is Max-1-EllipsisLength,
        between(NewTagLength, Spaces, _), write(' '), fail;
        true
    ),
    ansi_format([bold, fg(Color)], Ellipsis, [])
    .

% log a message and a tag at different levels
log_v(Tag, Log) :- log_at(verbose, Tag, Log).
log_d(Tag, Log) :- log_at(debug, Tag, Log).
log_i(Tag, Log) :- log_at(info, Tag, Log).
log_w(Tag, Log) :- log_at(warn, Tag, Log).
log_e(Tag, Log) :- log_at(error, Tag, Log).
log_wtf(Tag, Log) :- log_at(assert, Tag, Log).

% log a message at different levels
log_v(Log) :- log_v('', Log).
log_d(Log) :- log_d('', Log).
log_i(Log) :- log_i('', Log).
log_w(Log) :- log_w('', Log).
log_e(Log) :- log_e('', Log).
log_wtf(Log) :- log_wtf('', Log).

% test the logging system
test_log :-
    log_v(verboseLog, 'this is a verbose log'),
    log_d(debugLog, 'this is a debug log'),
    log_i('a very long info tag', 'this is an info log with a very long tag'),
    log_w('warnFunction', 'some warn log'),
    log_e('ErrorPredicate', 'some other error log'),
    log_wtf('WHATTHEFUCK', 'what the fuck error log').

% ---------------- %
%    LOG FILTER    %
% ---------------- %

/**
 * Set the log level, any level lesser than this will not be printed.
 * This is a setter that checks that the input parameter is instantiated
 * and that the specified level exists.
 * 
 * @param Level the level to set the logging information to.
 */
log_level(Level) :-
    not(var(Level)),
    (
        log_level(_, Level, _, _) ->
            true;
            log_w('log', ['Log with level ', Level, ' not found!']),
            fail
    ),
    retractall(log_min(_)),
    assertz(log_min(Level)), !.             % cut so it doesn't fall into the level getter

/**
 * Return the current log level, checking that the input parameter is an
 * unbound variable that can therefore be instantiated.
 * 
 * @return Level the current minumum log level.
 */
log_level(Level) :-
    var(Level),
    log_min(X),
    Level = X.

/**
 * Checks if a specified level can be currently printed.
 * Succeed if the input log has a higher or equal priority than the current minimum level.
 * Fails otherwise
 * 
 * @param Level to be checked.
 */
can_log(Level) :-
    log_min(CurrentLevel),
    log_level(CurrentPriority, CurrentLevel, _, _),
    log_level(Priority, Level, _, _),
    Priority >= CurrentPriority.

% at the beginning, show everything
:- log_level(verbose).