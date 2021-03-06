:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(27599, 28108.1))]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 7, 8, 9, 10]) :-
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
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(210.0, 213.0)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(115.0, 123.1)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(131.2, 139.29999999999998)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(139.29999999999998,
					       147.39999999999998)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(147.39999999999998,
					       155.49999999999997)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(155.49999999999997,
					       163.59999999999997)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(163.59999999999997,
					       171.69999999999996)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(171.69999999999996,
					       179.79999999999995)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(179.79999999999995,
					       187.89999999999995)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(187.89999999999995,
					       195.99999999999994)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(195.99999999999994,
					       204.09999999999994)),
			       condition('PatientAge',
					 range(28108.1, 28617.199999999997))
			     ]).

