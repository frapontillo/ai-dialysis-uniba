/** <module> Utility module

Utility methods: time, list, print, math.

@author Francesco Pontillo
@license Apache License, Version 2.0
*/

% --------------------------------------------------------------------------- %
%                                     TIMER                                   %
% --------------------------------------------------------------------------- %

/**
 * measure_time is det.
 * 
 * Reset the global timer.
 */
measure_time :- measure_time(_), !.
measure_time.

/**
 * measure_time(-Time) is semidet.
 * 
 * Return the time (in milliseconds) elapsed from the last call to measure_time/0.
 * 
 * @param Time            The elapsed time, in milliseconds.
 */
measure_time(Time) :- statistics(walltime, [_ | [Time]]), !.
measure_time(_).

/**
 * timer_time(?Name, ?Time) is nondet.
 * 
 * Holds information for existing timers.
 * 
 * @param TimerName       The name of the timer.
 * @param TimerStart      The date (in seconds, UNIX epoch time) when the timer was started.
 */
:- dynamic timer_time/2.

/**
 * timer_start(+Name) is semidet.
 * 
 * Start a new timer with a given name.
 * If another timer with the given name exists, an error is raised.
 * 
 * @param Name            The name of the new timer.
 */
timer_start(Name) :-
    timer_must_not_exist(Name),
    statistics(real_time,[Time|_]),
    assertz(timer_time(Name, Time)).

/**
 * timer_get(+Name, -Elapsed) is semidet.
 * 
 * Get the elapsed time from a given timer.
 * 
 * @param Name            The name of the timer.
 * @param Elapsed         The number of elapsed seconds since the timer was started.
 */
timer_get(Name, Elapsed) :-
    timer_must_exist(Name),
    timer_time(Name, Time),
    statistics(real_time,[NowTime|_]),
    Elapsed is NowTime - Time.

/**
 * timer_stop(+Name, -Elapsed) is semidet.
 * 
 * Stop and destroy a given timer.
 * 
 * @param Name            The name of the timer to be stopped.
 * @param Elapsed         The number of elapsed seconds since the timer was started.
 */
timer_stop(Name, Elapsed) :-
    timer_must_exist(Name),
    timer_get(Name, Elapsed),
    retractall(timer_time(Name, _)).

/**
 * timer_must_not_exist(+Name) is semidet.
 * 
 * Check if a timer exists and logs an error if it does.
 *
 * @param Name            The name of the timer to check for.
 */
timer_must_not_exist(Name) :-
    ( timer_time(Name, _) -> 
        log_e('timer', ['A timer with the name ', Name, ' already exists.']), fail; true ).

/**
 * timer_must_exist(+Name) is semidet.
 * 
 * Check if a timer doesn't exists and logs an error if it does not.
 *
 * @param Name            The name of the timer to check for.
 */
timer_must_exist(Name) :-
    ( not(timer_time(Name, _)) -> 
        log_e('timer', ['A timer with the name ', Name, ' does not exists.']), fail; true ).

% --------------------------------------------------------------------------- %
%                                     TIME                                    %
% --------------------------------------------------------------------------- %

/**
 * format_ms(?Time, ?Minutes, ?Seconds, ?Milliseconds) is semidet.
 * 
 * Convert a bunch of milliseconds into minutes, seconds an milliseconds.
 * 
 * @param Time              The time in milliseconds.
 * @param Minutes           The number of minutes in Time.
 * @param Seconds           The number of seconds in Time.
 * @param Milliseconds      The number of remaining milliseconds in Time.
 */
format_ms(Time, Minutes, Seconds, Milliseconds) :-
    % calculate the minutes
    MinutesFloat is (Time / (1000*60)),
    Minutes is floor(MinutesFloat),
    % calculate the seconds
    SecondsFloat is (Time mod (1000*60) / 1000),
    Seconds is floor(SecondsFloat),
    % calculate the milliseconds
    Milliseconds is (Time mod 1000)
    .

/**
 * format_s(?Time, ?String) is semidet.
 * 
 * Convert a bunch of seconds into a string representation.
 * 
 * @param Time              The time in seconds.
 * @param Minutes           The string representation of Time, in the '{M}m {S}s {MS}ms' format.
 */
format_s(Time, String) :-
    Ms is (Time * 1000),
    format_ms(Ms, String).

/**
 * format_ms(?Time, ?String) is semidet.
 * 
 * Convert a bunch of seconds into a string representation.
 * 
 * @param Time              The time in milliseconds.
 * @param Minutes           The string representation of Time, in the '{M}m {S}s {MS}ms' format.
 */
format_ms(Time, String) :-
    format_ms(Time, Minutes, Seconds, Milliseconds),
    TimeList = [Minutes, 'm ', Seconds, 's ', Milliseconds, 'ms'],
    atomic_list_concat(TimeList, String)
    .

% --------------------------------------------------------------------------- %
%                                  PRINT UTIL                                 %
% --------------------------------------------------------------------------- %
/*
 * println(+List, +Separator) is det.
 * 
 * Print a List and separate each element with Separator. It always add a new line in the end.
 * 
 * @param List              The list to be printed. This can also be a simple element.
 * @param Separator         The separator to be used to separate each list element.
 */
println([], _) :- nl.
println([Head|Tail], Separator) :- write(Head), write(Separator), println(Tail, Separator), !.
println(Element, Separator) :- not(Element = [_|_]), println([Element], Separator).
/**
 * println(+Element) is det.
 * 
 * Print something without a separator.
 * 
 * @param Element           The element to print.
 * @see println/2.
 */
println(Element) :- println(Element, '').

% --------------------------------------------------------------------------- %
%                                  LIST UTIL                                  %
% --------------------------------------------------------------------------- %

/**
 * list_min(+List:list, ?Min) is semidet.
 * 
 * Get the minimum element from a numeric list.
 * 
 * @param List              The list to look the minimum into.
 * @param Min               The minimum element of the list.
 */
% start condition, Min is a variable, start with Min = Head
list_min([Head|Tail], Min) :- number(Head), CurMin = Head, list_min(Tail, CurMin, Min), !.
list_min([_Head|Tail], Min) :- list_min(Tail, Min), !.
% end condition, return the CurMin
list_min([], CurMin, Min) :- Min = CurMin.
% loop condition with a min update
list_min([Head|Tail], CurMin, FinalMin) :-
  number(Head), <(Head, CurMin), list_min(Tail, Head, FinalMin), !.
% loop condition without a min update (Head >= CurMax OR Head NaN)
list_min([_Head|Tail], CurMin, FinalMin) :- list_min(Tail, CurMin, FinalMin), !.

/**
 * list_max(+List:list, ?Max) is semidet.
 * 
 * Get the maximum element from a numeric list.
 * 
 * @param List              The list to look the minimum into.
 * @param Max               The maximum element of the list.
 */
% start condition, Max is a variable, start with Max = Head
list_max([Head|Tail], Max) :- number(Head), CurMax = Head, list_max(Tail, CurMax, Max), !.
list_max([_Head|Tail], Max) :- list_max(Tail, Max), !.
% end condition, return the CurMax
list_max([], CurMax, Max) :- Max = CurMax.
% loop condition with a max update
list_max([Head|Tail], CurMax, FinalMax) :-
  number(Head), >(Head, CurMax), list_max(Tail, Head, FinalMax), !.
% loop condition without a max update (Head < CurMax OR Head NaN)
list_max([_Head|Tail], CurMax, FinalMax) :- list_max(Tail, CurMax, FinalMax), !.

/**
 * list_most_common(+List:list, ?Element, ?Count) is semidet.
 * 
 * Get the most common Element from a List, returning the Count.
 * 
 * @param List              The list to look into.
 * @param Element           The most common element of the list.
 * @param Count             The number of times the most common element was found in the list.
 */
list_most_common(List, Element, Count) :-
    % remove duplicates
    setof(X, member(X,List), Set),
    % make a list of count(Element, Count)
    findall(
        count(TempElement, TempCount),
        (member(TempElement, Set), aggregate_all(count, member(TempElement, List), TempCount)),
        AllCounts),
    aggregate_all(
        max(MaxCount, MaxElement), member(count(MaxElement, MaxCount), AllCounts), Result),
    Result = max(Count, Element).

/**
 * index_of(+List:list, +Element, ?Index) is semidet.
 * 
 * Get the position of an element in a list.
 * 
 * @param List              The list to look into.
 * @param Element           The element to find the position for.
 * @param Index             The index of the element in the list.
 */
index_of([], _, _):- fail.
index_of([Element|_], Element, 0):- !.
index_of([_|Tail], Element, Index):-
    index_of(Tail, Element, OtherIndex), !,
    Index is OtherIndex + 1.

/**
 * list_push(+List:list, +Element, -ResultingList:list) is det.
 * 
 * Add an element to the end of a list.
 * 
 * @param List              The list to append the element to.
 * @param Element           The element to be appended.
 * @param ResultingList     The resulting list.
 */
% appending an element to an empty list results in a list with that only element
list_push([], Element, [Element]).
% appending an Element to a list made by Head|Tail results in a list with:
%  - the same Head
%  - its NewTail made by appending Tail and Element
list_push([Head|Tail], Element, [Head|NewTail]) :- list_push(Tail, Element, NewTail).

/**
 * list_unshift(+List:list, +Element, -ResultingList:list) is det.
 * 
 * Add an element to the top of a list.
 * 
 * @param List              The list to unshift the element to.
 * @param Element           The element to be unshifted.
 * @param NewList           The resulting list.
 */
list_unshift(List, Element, NewList) :-
    NewList = [Element|List].

/**
 * list_append(+A, +B, -ResultingList:list) is det.
 * 
 * Concat two generic elements in one list. Both elements can be lists or atoms, indipendently.
 * 
 * @param A                 The first element (atom or list).
 * @param B                 The second element (atom or list).
 * @param List              The resulting list.
 */
list_append(A, B, List) :-
    (not(is_list(A)) -> NewA = [A]; NewA = A),
    (not(is_list(B)) -> NewB = [B]; NewB = B),
    append([NewA, NewB], List).
/*
 * remove(+Element, +List:list, -ResultingList:list) is det.
 *
 * Remove an element from a list.
 * 
 * @param List              The list to remove the element from.
 * @param Element           The element to be removed.
 * @param ResultingList     The resulting list.
 */
% removing an Element from an empty List returns an empty list
list_remove([], _, []).
% if the Element is the Head of the list, return NewTail,
% which is the Tail after calling remove on it again
list_remove([Element|Tail], Element, NewTail) :- list_remove(Tail, Element, NewTail), !.
% if the Element is not the Head of the list, return the original Head and a NewTail,
% which is the Tail after calling remove on it again
list_remove([Head|Tail], Element, [Head|NewTail]) :- list_remove(Tail, Element, NewTail).

/*
 * concat_string_list(+List:list, -Result) is det.
 *
 * Concat elements of a list in a string.
 * 
 * @param List              The list to get the elements from.
 * @param Result            The resulting string.
 */
% concat_string_list/2 is used to begin the string concatenation
concat_string_list([Head|Tail], Result) :- concat_string_list([Head|Tail], '', Result).
% concat_string_list/3 is used to recurse the string concatenation
concat_string_list([Head|Tail], Temp, Result) :-
  string_concat(Temp, Head, NewString),
  concat_string_list(Tail, NewString, Result).
concat_string_list([], Temp, Result) :- Result = Temp.

% --------------------------------------------------------------------------- %
%                                 MATHEMATICS                                 %
% --------------------------------------------------------------------------- %

/**
 * log2(+Expr, -R) is det.
 * 
 * Calculate the base-2 logarithm of Expr.
 *
 * @param Expr              The expression to calculate the log2 for.
 * @param R                 The result of the calculation.
 */
log2(Expr, R) :- R is log10(Expr) / log10(2).