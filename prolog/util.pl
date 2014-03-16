% ---------------- %
%  EXECUTION TIME  %
% ---------------- %
measure_time(Time) :- statistics(walltime, [_ | [Time]]), !.
measure_time(_).
measure_time :- measure_time(_), !.
measure_time.

% ---------------- %
%    PRINT LIST    %
% ---------------- %
println([]) :- nl.
println([Head|Tail]) :- write(Head), println(Tail), !.
println(Element) :- not(Element = [Head|Tail]), println([Element]).


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