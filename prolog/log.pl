% ---------------- %
%        LOG       %
% ---------------- %

log_level(2, verbose, ' V ', [bold,bg(default)]).
log_level(3, debug, ' D ', [bold,bg(cyan)]).
log_level(4, info, ' I ', [bold,bg(green)]).
log_level(5, warn, ' W ', [bold,bg(yellow)]).
log_level(6, error, ' E ', [bold,bg(red)]).
log_level(7, assert, ' A ', [bold,bg(magenta)]).

log_at(Level, Log) :-
    log_at(Level, '', Log).

log_at(Level, Tag, Log) :-
    % log the tag
    log_tag(Level, Tag, 13),
    % get the level
    log_level(_, Level, LevelText, Format),
    % log the level type
    write(' '),
    ansi_format(Format, LevelText, []),
    write(' '),
    % log the actual message
    println(Log).

% log the tag with a given level and a maximum length
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

log_v(Tag, Log) :- log_at(verbose, Tag, Log).
log_d(Tag, Log) :- log_at(debug, Tag, Log).
log_i(Tag, Log) :- log_at(info, Tag, Log).
log_w(Tag, Log) :- log_at(warn, Tag, Log).
log_e(Tag, Log) :- log_at(error, Tag, Log).
log_wtf(Tag, Log) :- log_at(assert, Tag, Log).

log_v(Log) :- log_v('', Log).
log_d(Log) :- log_d('', Log).
log_i(Log) :- log_i('', Log).
log_w(Log) :- log_w('', Log).
log_e(Log) :- log_e('', Log).
log_wtf(Log) :- log_wtf('', Log).

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
% at the beginning, show everything
log_min(simple).