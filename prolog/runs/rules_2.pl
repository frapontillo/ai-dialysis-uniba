:- dynamic final_positive/2.

final_positive(A, [1, 3, 4, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('DAPAverage',
					 range(112.79999999999998,
					       122.39999999999998)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [3, 4, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('DAPAverage',
					 range(160.79999999999995,
					       170.39999999999995)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(51.599999999999994,
					       56.39999999999999)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(75.59999999999998,
					       80.39999999999998)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(89.99999999999997,
					       94.79999999999997)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(89.99999999999997,
					       94.79999999999997)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(17150, 18449.1))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(18449.1, 19748.199999999997))
			     ]).
final_positive(A, [1, 2, 3, 4, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(22346.399999999994,
					       23645.499999999993))
			     ]).
final_positive(A, [1, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23645.499999999993,
					       24944.59999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(24944.59999999999,
					       26243.69999999999))
			     ]).
final_positive(A, [1, 2, 3, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(26243.69999999999,
					       27542.79999999999))
			     ]).
final_positive(A, [9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('DAPStart',
					 range(51.599999999999994,
					       56.39999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [1, 4, 6, 8]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(1, 1)),
			       condition('DAPAverage',
					 range(151.19999999999996,
					       160.79999999999995)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(180.0, 192.0)),
			       condition('DAPStart',
					 range(56.39999999999999,
					       61.19999999999999)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(180.0, 192.0)),
			       condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [2]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(1.366, 1.854)),
			       condition('SAPEnd',
					 range(145.8, 158.10000000000002)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [8]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.342, 2.83)),
			       condition('DAPAverage',
					 range(141.59999999999997,
					       151.19999999999996)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.342, 2.83)),
			       condition('DAPStart',
					 range(75.59999999999998,
					       80.39999999999998)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [2]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(133.5, 145.8)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).
final_positive(A, [1, 3, 4]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(118.8, 127.6)),
			       condition('DAPAverage',
					 range(141.59999999999997,
					       151.19999999999996)),
			       condition('DeltaBloodFlow',
					 range(-5.800000000000001,
					       6.399999999999999)),
			       condition('PatientAge',
					 range(28841.899999999987,
					       30140.999999999985))
			     ]).

