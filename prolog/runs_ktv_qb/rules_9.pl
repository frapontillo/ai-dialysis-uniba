:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(1.39, 9.351)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(9.351, 17.312)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(17.312, 25.273000000000003)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(25.273000000000003, 33.234)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(33.234, 41.195)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(41.195, 49.156)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(49.156, 57.117)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(57.117, 65.078)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(65.078, 73.039)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('KTV', range(73.039, 81.0)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(27599, 28108.1))]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(32689.999999999985,
					       33199.099999999984))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(1, 1)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).

