:- dynamic final_positive/2.

final_positive(A, [10]) :-
	check_condition_list(A, [condition('KTV', range(81.0, 88.961))]).
final_positive(A, [1, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23645.499999999993,
					       24944.59999999999)),
			       condition('KTV', range(81.0, 88.961))
			     ]).

