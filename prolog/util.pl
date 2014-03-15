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