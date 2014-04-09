:- dynamic final_positive/2.

final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd', range(20.0, 29.6)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(39.2, 48.800000000000004)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(48.800000000000004,
					       58.400000000000006)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(58.400000000000006, 68.0)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [6]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd', range(68.0, 77.6)),
			       condition('PatientSex', range(0, 0)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd', range(68.0, 77.6)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 4, 8]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd', range(68.0, 77.6)),
			       condition('ProgWeightLoss',
					 range(1.8029999999999997,
					       2.3739999999999997)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(87.19999999999999,
					       96.79999999999998)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(96.79999999999998,
					       106.39999999999998)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(106.39999999999998,
					       115.99999999999997)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 10]) :-
	check_condition_list(A,
			     
			     [ condition('DAPEnd',
					 range(115.99999999999997,
					       125.59999999999997)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart', range(68.2, 77.9)),
			       condition('ProgWeightLoss',
					 range(1.8029999999999997,
					       2.3739999999999997)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart', range(68.2, 77.9)),
			       condition('SAPEnd',
					 range(136.50000000000003,
					       149.40000000000003)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('DAPStart',
					 range(77.9, 87.60000000000001)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(-34.0, -20.2)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(-20.2, -6.399999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(7.400000000000002,
					       21.200000000000003)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(21.200000000000003, 35.0)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow', range(35.0, 48.8)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(62.599999999999994,
					       76.39999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(76.39999999999999,
					       90.19999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(90.19999999999999,
					       103.99999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [9]) :-
	check_condition_list(A,
			     
			     [ condition('DeltaBloodFlow',
					 range(103.99999999999999,
					       117.79999999999998)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 2, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(13755, 15555.4))]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     [condition('PatientAge', range(17355.8, 19156.2))]).
final_positive(A, [1, 2, 3, 4, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(19156.2, 20956.600000000002))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(20956.600000000002,
					       22757.000000000004))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(22757.000000000004,
					       24557.400000000005))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(24557.400000000005,
					       26357.800000000007))
			     ]).
final_positive(A, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientAge',
					 range(26357.800000000007,
					       28158.200000000008))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('DAPEnd', range(77.6, 87.19999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('PatientSex', range(0, 0)),
			       condition('RealDuration',
					 range(208.79999999999995,
					       223.99999999999994)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('ProgDuration', range(180.0, 192.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(0.09, 0.6609999999999999)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(0.6609999999999999,
					       1.2319999999999998)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(1.2319999999999998,
					       1.8029999999999997)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(1.8029999999999997,
					       2.3739999999999997)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.3739999999999997,
					       2.9449999999999994)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.9449999999999994,
					       3.515999999999999)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 3, 4, 5, 6, 7, 8]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.9449999999999994,
					       3.515999999999999)),
			       condition('DAPEnd', range(77.6, 87.19999999999999)),
			       condition('ProgDuration', range(240.0, 252.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [1, 4, 8, 9]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(2.9449999999999994,
					       3.515999999999999)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(3.515999999999999,
					       4.086999999999999)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(4.086999999999999,
					       4.657999999999999)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(4.657999999999999,
					       5.228999999999998)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(5.228999999999998,
					       5.799999999999998)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 7]) :-
	check_condition_list(A,
			     
			     [ condition('ProgWeightLoss',
					 range(5.799999999999998,
					       6.370999999999998)),
			       condition('DAPEnd', range(68.0, 77.6)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration', range(148.0, 163.2)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(163.2, 178.39999999999998)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(178.39999999999998,
					       193.59999999999997)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(193.59999999999997,
					       208.79999999999995)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(239.19999999999993,
					       254.39999999999992)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(254.39999999999992,
					       269.5999999999999)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(269.5999999999999,
					       284.7999999999999)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(284.7999999999999,
					       299.9999999999999)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [2, 3, 4, 5, 6, 7, 8, 9, 10]) :-
	check_condition_list(A,
			     
			     [ condition('RealDuration',
					 range(299.9999999999999,
					       315.1999999999999)),
			       condition('PatientAge',
					 range(29958.60000000001,
					       31759.00000000001))
			     ]).
final_positive(A, [10]) :-
	check_condition_list(A,
			     
			     [ condition('RealWeightLoss',
					 range(2.0700000000000003, 2.636)),
			       condition('SAPStart', range(106.4, 156.8)),
			       condition('DAPStart',
					 range(77.9, 87.60000000000001)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [3]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(123.60000000000002,
					       136.50000000000003)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).
final_positive(A, [5]) :-
	check_condition_list(A,
			     
			     [ condition('SAPEnd',
					 range(136.50000000000003,
					       149.40000000000003)),
			       condition('DAPStart', range(68.2, 77.9)),
			       condition('DeltaBloodFlow',
					 range(-6.399999999999999,
					       7.400000000000002)),
			       condition('ProgDuration', range(204.0, 216.0)),
			       condition('PatientAge',
					 range(28158.200000000008,
					       29958.60000000001))
			     ]).

