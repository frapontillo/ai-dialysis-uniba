/**
 * Base-2 logarithm.
 *
 * @param Expr the expression to calculate the log2 for.
 * @return R the result of the calculation.
 */
log2(Expr, R) :- R is log10(Expr) / log10(2).

/**
 * Calculate the entropy of a given list of examples (by IDs).
 * 
 * @param IncludedValues 	A list of the example IDs to be included when considering the entropy calculus.
 *											 	The resulting examples will be intersected with the positive and negative examples.
 *											 	If you don't want to filter anything, pass in all of the example IDs.
 *												e.g.	findall(ID, (example(_, ID, 'ID', ID), example(_, ID, 'DeltaWeight', 1)), IncludedValues).
 * @return Entropy 				The resulting entropy of the examples passed by IDs.
 */
entropy(IncludedValues, Entropy) :- 
	findall(ID, example(positive, ID, 'ID', ID), PositiveList),
	intersection(PositiveList, IncludedValues, PositiveSubset),
	length(PositiveSubset, PositiveCount),																							% calculate the number of positive examples (filtered)

	findall(ID, example(negative, ID, 'ID', ID), NegativeList),
	intersection(NegativeList, IncludedValues, NegativeSubset),
	length(NegativeSubset, NegativeCount),																							% calculate the number of negative examples (filtered)

	findall(ID, example(_, ID, 'ID', ID), CompleteSet),
	intersection(CompleteSet, IncludedValues, Subset),
	length(Subset, SubsetCount),																												% calculate the number of all the examples (filtered)

	P is PositiveCount / SubsetCount,
	N is NegativeCount / SubsetCount,
	catch(
		(log2(P, LogP),																																		% if there's an error calculating the logarithm, assume
		log2(N, LogN),																																		% it's because P or N are 0, so the entropy is 0
		Entropy is (-P*LogP)+(-N*LogN)),
		_,
		Entropy is 0
	)
	.

% 
/**
 * Calculate the entropy of the whole set of examples.
 * 
 * @return Entropy 				The resulting entropy of the whole set of examples.
 */
entropy(Entropy) :-
	findall(ID, example(_, ID, 'ID', ID), CompleteSet),
	entropy(CompleteSet, Entropy).