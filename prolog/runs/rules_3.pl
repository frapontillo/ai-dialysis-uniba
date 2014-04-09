:- dynamic final_positive/2.

final_positive(A, [10]) :-
	check_condition_list(A, [condition('KTV', range(81.0, 88.961))]).
final_positive(A, [1, 2, 3, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23731.999999999993,
					       25444.59999999999)),
			       condition('KTV', range(81.0, 88.961))
			     ]).

