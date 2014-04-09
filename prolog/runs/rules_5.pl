:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(0.0, 9.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(9.0, 18.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(18.0, 27.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(27.0, 36.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(36.0, 45.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(45.0, 54.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(63.0, 72.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(72.0, 81.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(81.0, 90.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('QB', range(90.0, 99.0)),
			       condition('KTV', range(81.0, 88.961))
			     ]).

