:- dynamic final_positive/2.

final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(73.19999999999999,
					       80.99999999999999)),
			       condition('PatientSex', range(0, 0)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(73.19999999999999,
					       80.99999999999999)),
			       condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [1, 2, 3, 4, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(15169, 16881.6))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(18594.199999999997,
					       20306.799999999996))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(20306.799999999996,
					       22019.399999999994))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(22019.399999999994,
					       23731.999999999993))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23731.999999999993,
					       25444.59999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(25444.59999999999,
					       27157.19999999999))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(30582.399999999987,
					       32294.999999999985))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(32294.999999999985,
					       34007.599999999984))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(180.0, 192.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(0.39, 0.881)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(0.881, 1.3719999999999999)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(1.863, 2.354)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.354, 2.845)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.845, 3.3360000000000003)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.3360000000000003,
					       3.8270000000000004)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.8270000000000004,
					       4.3180000000000005)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(4.3180000000000005, 4.809)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(4.809, 5.3)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(5.3, 5.7909999999999995)),
			       condition('PatientAge',
					 range(27157.19999999999,
					       28869.79999999999))
			     ]).
final_positive(A, [1, 3, 4, 6, 7, 8]) :-
	check_condition_list(A,
			     
			     [ condition('RealWeightLoss',
					 range(2.1199999999999997,
					       2.6959999999999997)),
			       condition('SAPStart',
					 range(149.9, 173.20000000000002)),
			       condition('PatientSex', range(0, 0)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).
final_positive(A, [2]) :-
	check_condition_list(A,
			     
			     [ condition('SAPAverage',
					 range(273.20000000000005,
					       301.50000000000006)),
			       condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28869.79999999999,
					       30582.399999999987))
			     ]).

