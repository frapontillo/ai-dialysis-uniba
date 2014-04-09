:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(13225, 15026.2))]).
final_positive(A, [1, 2, 3, 4, 5, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(16827.4, 18628.600000000002))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(18628.600000000002,
					       20429.800000000003))
			     ]).
final_positive(A, [1, 2, 3, 4, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(20429.800000000003,
					       22231.000000000004))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(22231.000000000004,
					       24032.200000000004))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(24032.200000000004,
					       25833.400000000005))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(25833.400000000005,
					       27634.600000000006))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('SAPStart',
					 range(152.8, 163.60000000000002)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(-1.7, -1.0030000000000001)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(-1.0030000000000001,
					       -0.30600000000000016)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(-0.30600000000000016,
					       0.3909999999999998)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(0.3909999999999998,
					       1.0879999999999996)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [6]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(0.3909999999999998,
					       1.0879999999999996)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(1.7849999999999997,
					       2.4819999999999998)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.4819999999999998, 3.179)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(3.179, 3.876)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 7, 8]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(3.179, 3.876)),
			       condition('PatientSex', range(0, 0)),
			       condition('SAPStart',
					 range(152.8, 163.60000000000002)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.876, 4.5729999999999995)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(4.5729999999999995, 5.27)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(5.27, 5.967)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(72.0, 84.3)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(72.0, 84.3)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(84.3, 96.6)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(84.3, 96.6)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(96.6, 108.89999999999999)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(96.6, 108.89999999999999)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(108.89999999999999,
					       121.19999999999999)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(121.19999999999999, 133.5)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(133.5, 145.8)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd', range(133.5, 145.8)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(145.8, 158.10000000000002)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(158.10000000000002,
					       170.40000000000003)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(158.10000000000002,
					       170.40000000000003)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(170.40000000000003,
					       182.70000000000005)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(170.40000000000003,
					       182.70000000000005)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(182.70000000000005,
					       195.00000000000006)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 5, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(182.70000000000005,
					       195.00000000000006)),
			       condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(88.0, 98.8)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(98.8, 109.6)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(109.6, 120.39999999999999)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(120.39999999999999, 131.2)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(131.2, 142.0)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(142.0, 152.8)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(152.8, 163.60000000000002)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [6]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(152.8, 163.60000000000002)),
			       condition('PatientSex', range(0, 0)),
			       condition('ProgWeightLoss', range(3.179, 3.876)),
			       condition('PatientAge',
					 range(29435.800000000007,
					       31237.000000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(174.40000000000003,
					       185.20000000000005)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).
final_positive(A, [8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(185.20000000000005,
					       196.00000000000006)),
			       condition('PatientAge',
					 range(27634.600000000006,
					       29435.800000000007))
			     ]).

