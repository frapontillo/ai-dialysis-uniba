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

/**
 * Calculate the Information Gain for a set of values and a given attribute.
 *
 * @param IncludedValues 	A list of IDs to be included when considering the information gain calculus.
 * @param Attribute 			The attribute to calculate the information gain for.
 * @Return InfoGain 			The calculated information gain.
 */
info_gain(IncludedValues, Attribute, InfoGain) :- 
	% first off, let's calculate the total entropy
	entropy(IncludedValues, TotalEntropy),
	% calculate the partial information gain on each split of the Attribute (category or class)
	PartialGains = (
		class(Attribute, RangeList),																											% get the list of splits for the given Attribute
		member(Range, RangeList),																													% loop through every range in the list
		partial_info_gain(IncludedValues, Attribute, Range, PartialInfoGain)							% calculate the current partial info gain
	),
	% sum all of the partial gains
	findall(PartialInfoGain, PartialGains, GainList),
	sum_list(GainList, PartialGainSum),
	InfoGain is TotalEntropy - PartialGainSum
	.

partial_info_gain(IncludedValues, Attribute, Range, PartialInfoGain) :-
	% get all the examples that satisfy the given Range
	CandidateValues = (
		example(_, ID, Attribute, Value),
		Range = range(Bottom, Top),
		Value >= Bottom,
		Value =< Top
	),
	% get a list of the IDs
	findall(ID, CandidateValues, IDList),
	% intersect and find the useful IDs
	intersection(IncludedValues, IDList, Values),
	% get the entropy of the subset
	entropy(Values, EntropyValues),
	% get the size of the subset
	length(Values, SizeValues),
	% get the size of the original set
	length(IDList, SizeIncludedValues),
	PartialInfoGain is (EntropyValues * SizeValues / SizeIncludedValues)
	.