% ---------------- %
%  EXECUTION TIME  %
% ---------------- %
measure_time(Time) :- statistics(walltime, [_ | [Time]]), !.
measure_time(_).
measure_time :- measure_time(_), !.
measure_time.

% ---------------- %
%   TIME CONVERT   %
% ---------------- %
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

format_ms(Time, String) :-
	format_ms(Time, Minutes, Seconds, Milliseconds),
	TimeList = [Minutes, 'm ', Seconds, 's ', Milliseconds, 'ms'],
	atomic_list_concat(TimeList, String)
	.

% ---------------- %
%    PRINT LIST    %
% ---------------- %
println([], _) :- nl.
println([Head|Tail], Separator) :- write(Head), write(Separator), println(Tail, Separator), !.
println(Element, Separator) :- not(Element = [_|_]), println([Element], Separator).
println(Element) :- println(Element, '').

% ---------------- %
%     LIST MIN     %
% ---------------- %
% start condition, Min is a variable, start with Min = Head
list_min([Head|Tail], Min) :- number(Head), CurMin = Head, list_min(Tail, CurMin, Min), !.
list_min([_Head|Tail], Min) :- list_min(Tail, Min), !.
% end condition, return the CurMin
list_min([], CurMin, Min) :- Min = CurMin.
% loop condition with a min update
list_min([Head|Tail], CurMin, FinalMin) :- number(Head), <(Head, CurMin), list_min(Tail, Head, FinalMin), !.
% loop condition without a min update (Head >= CurMax OR Head NaN)
list_min([_Head|Tail], CurMin, FinalMin) :- list_min(Tail, CurMin, FinalMin), !.

% ---------------- %
%     LIST MAX     %
% ---------------- %
% start condition, Max is a variable, start with Max = Head
list_max([Head|Tail], Max) :- number(Head), CurMax = Head, list_max(Tail, CurMax, Max), !.
list_max([_Head|Tail], Max) :- list_max(Tail, Max), !.
% end condition, return the CurMax
list_max([], CurMax, Max) :- Max = CurMax.
% loop condition with a max update
list_max([Head|Tail], CurMax, FinalMax) :- number(Head), >(Head, CurMax), list_max(Tail, Head, FinalMax), !.
% loop condition without a max update (Head < CurMax OR Head NaN)
list_max([_Head|Tail], CurMax, FinalMax) :- list_max(Tail, CurMax, FinalMax), !.

% ---------------- %
%  STRING CONCAT   %
% ---------------- %
% concat_string_list/2 is used to begin the string concatenation
concat_string_list([Head|Tail], Result) :- concat_string_list([Head|Tail], '', Result).
% concat_string_list/3 is used to recurse the string concatenation
concat_string_list([Head|Tail], Temp, Result) :-
	string_concat(Temp, Head, NewString),
	concat_string_list(Tail, NewString, Result).
concat_string_list([], Temp, Result) :- Result = Temp.

/**
 * list_push/3
 * 
 * @param A list to append the element to
 * @param The element to be appended
 * @param The resulting list
 */
% appending an element to an empty list results in a list with that only element
list_push([], Element, [Element]).
% appending an Element to a list made by Head|Tail results in a list with:
%  - the same Head
%  - its NewTail made by appending Tail and Element
list_push([Head|Tail], Element, [Head|NewTail]) :- list_push(Tail, Element, NewTail).

%* concat two generic elements in one list
list_append(A, B, List) :-
    (not(is_list(A)) -> NewA = [A]; NewA = A),
    (not(is_list(B)) -> NewB = [B]; NewB = B),
    append([NewA, NewB], List).

% ---------------- %
%    MATHEMATICS   %
% ---------------- %
/**
 * Base-2 logarithm.
 *
 * @param Expr the expression to calculate the log2 for.
 * @return R the result of the calculation.
 */
log2(Expr, R) :- R is log10(Expr) / log10(2).