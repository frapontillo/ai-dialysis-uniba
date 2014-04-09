:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 6, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPAverage', range(150.0, 164.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [1, 2, 3, 4, 6, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPAverage', range(164.0, 178.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart', range(80.0, 88.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(-20.0, -7.6)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(4.800000000000001,
					       17.200000000000003)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(17.200000000000003, 29.6)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(29.6, 42.0)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(54.4, 66.8)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(66.8, 79.2)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(79.2, 91.60000000000001)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [1, 2, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(91.60000000000001,
					       104.00000000000001)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaWeight',
					 range(0.2682240000000009,
					       0.9892520000000009)),
			       condition('SAPStart',
					 range(155.2, 166.39999999999998)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(13395, 15636))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(15636, 17877))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(17877, 20118))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(20118, 22359))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(22359, 24600))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(24600, 26841))]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(26841, 29082))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(31323, 33564))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(35805, 38046))]).
final_positive(A, [1, 2, 3, 4, 6, 9]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('DAPAverage', range(122.0, 136.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('SAPStart',
					 range(177.59999999999997,
					       188.79999999999995)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [1, 2]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(1, 1)),
			       condition('DeltaBloodFlow',
					 range(91.60000000000001,
					       104.00000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [3, 4, 5, 6, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(240.0, 252.0)),
			       condition('DeltaBloodFlow',
					 range(91.60000000000001,
					       104.00000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.6639999999999997,
					       3.5699999999999994)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('SAPStart', range(144.0, 155.2)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [1, 2, 3, 4, 6, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(195.0, 210.0)),
			       condition('DAPAverage', range(136.0, 150.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(195.0, 210.0)),
			       condition('DAPStart', range(64.0, 72.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [8]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(195.0, 210.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(270.0, 285.0)),
			       condition('DAPStart', range(64.0, 72.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [8]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(270.0, 285.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(88.0, 99.2)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(88.0, 99.2)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(99.2, 110.4)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(110.4, 121.60000000000001)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(132.8, 144.0)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart', range(144.0, 155.2)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(155.2, 166.39999999999998)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [8]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(155.2, 166.39999999999998)),
			       condition('RealDuration', range(210.0, 225.0)),
			       condition('DeltaBloodFlow',
					 range(-7.6, 4.800000000000001)),
			       condition('PatientAge', range(29082, 31323))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(166.39999999999998,
					       177.59999999999997)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(177.59999999999997,
					       188.79999999999995)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(188.79999999999995,
					       199.99999999999994)),
			       condition('PatientAge', range(26841, 29082))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPStart',
					 range(199.99999999999994,
					       211.19999999999993)),
			       condition('PatientAge', range(26841, 29082))
			     ]).

