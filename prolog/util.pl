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
println([], Separator) :- nl.
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
