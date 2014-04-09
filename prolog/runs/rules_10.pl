:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(12774, 14555.7))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(14555.7, 16337.400000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(16337.400000000001,
					       18119.100000000002))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(18119.100000000002,
					       19900.800000000003))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(19900.800000000003,
					       21682.500000000004))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(21682.500000000004,
					       23464.200000000004))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23464.200000000004,
					       25245.900000000005))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(25245.900000000005,
					       27027.600000000006))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(1, 1)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(1, 1)),
			       condition('SAPStart', range(103.2, 118.4)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(0.0, 0.78)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(0.0, 0.78)),
			       condition('SAPAverage', range(213.5, 242.0)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(1.56, 2.34)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.34, 3.12)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.12, 3.9000000000000004)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.9000000000000004,
					       4.680000000000001)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.9000000000000004,
					       4.680000000000001)),
			       condition('SAPStart',
					 range(179.19999999999996,
					       194.39999999999995)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(4.680000000000001,
					       5.460000000000001)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(5.460000000000001,
					       6.240000000000001)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(6.240000000000001,
					       7.020000000000001)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(7.020000000000001,
					       7.800000000000002)),
			       condition('PatientAge',
					 range(27027.600000000006,
					       28809.300000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 6, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPAverage', range(185.0, 213.5)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPAverage', range(356.0, 384.5)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(103.2, 118.4)),
			       condition('PatientAge',
					 range(28809.300000000007,
					       30591.000000000007))
			     ]).

