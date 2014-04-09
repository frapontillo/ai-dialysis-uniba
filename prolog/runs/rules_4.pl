:- dynamic final_positive/2.

final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(97.99999999999997,
					       103.59999999999997))
			     ]).
final_positive(A, [8]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(97.99999999999997,
					       103.59999999999997)),
			       condition('PatientAge',
					 range(27993.600000000006,
					       29650.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaDuration',
					 range(-0.8000000000000007,
					       6.299999999999999)),
			       condition('ProgWeightLoss', range(2.342, 2.83)),
			       condition('PatientSex', range(0, 0))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(14736, 16393.2))]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge', range(14736, 16393.2)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge', range(16393.2, 18050.4)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(18050.4, 19707.600000000002)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(19707.600000000002,
					       21364.800000000003)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(21364.800000000003,
					       23022.000000000004)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [1, 2, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23022.000000000004,
					       24679.200000000004))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(23022.000000000004,
					       24679.200000000004)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(24679.200000000004,
					       26336.400000000005))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(24679.200000000004,
					       26336.400000000005)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(26336.400000000005,
					       27993.600000000006)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(27993.600000000006,
					       29650.800000000007)),
			       condition('DAPStart',
					 range(86.79999999999998,
					       92.39999999999998))
			     ]).
final_positive(A, [1, 2, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('PatientAge',
					 range(29650.800000000007,
					       31308.000000000007))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('SAPEnd',
					 range(158.10000000000002,
					       170.40000000000003)),
			       condition('DAPStart', range(70.0, 75.6))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.83, 3.318)),
			       condition('PatientSex', range(0, 0))
			     ]).
final_positive(A, [1, 2, 4, 5]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(2.83, 3.318)),
			       condition('SAPEnd',
					 range(121.19999999999999, 133.5)),
			       condition('PatientAge',
					 range(27993.600000000006,
					       29650.800000000007))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss', range(4.782, 5.27)),
			       condition('PatientSex', range(0, 0))
			     ]).
final_positive(A, [1, 2, 4, 5, 6]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(158.10000000000002,
					       170.40000000000003)),
			       condition('PatientAge',
					 range(27993.600000000006,
					       29650.800000000007))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(124.4, 132.8)),
			       condition('DAPStart',
					 range(75.6, 81.19999999999999))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(132.8, 141.20000000000002)),
			       condition('PatientAge',
					 range(27993.600000000006,
					       29650.800000000007))
			     ]).

