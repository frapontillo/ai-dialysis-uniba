:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(23689, 24334.2))]).

