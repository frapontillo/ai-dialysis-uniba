:- dynamic final_positive/2.

final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(-18.0, -5.800000000000001)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(6.399999999999999,
					       18.599999999999998)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(18.599999999999998,
					       30.799999999999997)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(30.799999999999997, 43.0)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(55.2, 67.4)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(67.4, 79.60000000000001)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(79.60000000000001,
					       91.80000000000001)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(91.80000000000001,
					       104.00000000000001)),
			       condition('ProgDuration', range(240.0, 243.0))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(28108.1, 28617.199999999997)),
			       condition('KTV', range(1.39, 9.351))
			     ]).

