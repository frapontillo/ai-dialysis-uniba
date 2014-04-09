:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     [condition('KTV', range(17.312, 25.273000000000003))]).

