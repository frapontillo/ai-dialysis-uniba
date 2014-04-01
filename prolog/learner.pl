/**
 * Get the complete set of example IDs as a list.
 *
 * @return CompleteSet, list of all the example IDs.
 */
complete_set(CompleteSet) :-
    findall(ID, example(_, ID, 'ID', ID), CompleteSet).

/**
 * Calculate the entropy of a given list of examples (by IDs).
 * 
 * @param IncludedValues    A list of the example IDs to be included when considering the entropy calculus.
 *                          The resulting examples will be intersected with the positive and negative examples.
 *                          If you don't want to filter anything, pass in all of the example IDs.
 *                          e.g. findall(ID, (example(_, ID, 'ID', ID), example(_, ID, 'DeltaWeight', 1)), IncludedValues).
 * @return Entropy          The resulting entropy of the examples passed by IDs.
 */
entropy(IncludedValues, Entropy) :- 
    findall(ID, example(positive, ID, 'ID', ID), PositiveList),
    intersection(PositiveList, IncludedValues, PositiveSubset),
    length(PositiveSubset, PositiveCount),                                                                                          % calculate the number of positive examples (filtered)

    findall(ID, example(negative, ID, 'ID', ID), NegativeList),
    intersection(NegativeList, IncludedValues, NegativeSubset),
    length(NegativeSubset, NegativeCount),                                                                                          % calculate the number of negative examples (filtered)

    findall(ID, example(_, ID, 'ID', ID), CompleteSet),
    intersection(CompleteSet, IncludedValues, Subset),
    length(Subset, SubsetCount),                                                                                                    % calculate the number of all the examples (filtered)

    catch(
        (
            P is PositiveCount / SubsetCount,
            N is NegativeCount / SubsetCount,
            log2(P, LogP),                                                                                                          % if there's an error calculating the logarithm, assume
            log2(N, LogN),                                                                                                          % it's because P or N are 0, so the entropy is 0
            Entropy is (-P*LogP)+(-N*LogN)
        ),
        _, Entropy is 0
    )
    .

% 
/**
 * Calculate the entropy of the whole set of examples.
 * 
 * @return Entropy              The resulting entropy of the whole set of examples.
 */
entropy(Entropy) :-
    complete_set(CompleteSet),
    entropy(CompleteSet, Entropy).

/**
 * Select the best attribute from the given sets of attributes and examples,
 * using information gain.
 * 
 * @param Set           The set of example IDs to calculate the best attribute for.
 * @param Set           The set of attributes to select the best attribute from.
 * @return Attribute    The best attribute for the given set
 */
best_attribute(Set, Attributes, Attribute) :-
    measure_time,
    % collect the whole set of possible information gains for the given Set
    GainsSet = (target_class(Target), member(Attribute, Attributes), Attribute \= Target, info_gain(Set, Attribute, InfoGain)),
    % calculate the maximum InfoGain from GainsSet and get the Attribute
    aggregate_all(max(InfoGain, Attribute), GainsSet, BestSet),
    BestSet = max(InfoGain, Attribute),
    log_i('learner', ['Best info gain is achieved with attribute ', Attribute, ' with a value of ', InfoGain]),
    measure_time(Time), format_ms(Time, TimeString),
    log_d('learner', ['Best attribute calculus took ', TimeString, '.'])
    .

% shortcut for best_attribute to the complete set of examples and attributes
best_attribute(Attribute) :-
    complete_set(CompleteSet),
    findall(Attribute, data_type(Attribute, _), Attributes),
    best_attribute(CompleteSet, Attributes, Attribute)
    .

/**
 * Calculate the Information Gain for a set of values and a given attribute.
 *
 * @param Set                   A list of IDs to be included when considering the information gain calculus.
 * @param Attribute             The attribute to calculate the information gain for.
 * @Return InfoGain             The calculated information gain.
 */
info_gain(Set, Attribute, InfoGain) :- 
    log_v('learner', ['Calculating info gain for ', Attribute, '...']),
    % first off, let's calculate the total entropy
    entropy(Set, TotalEntropy),
    % calculate the partial information gain on each split of the Attribute (category or class)
    PartialGains = (
        class(Attribute, RangeList),                                                % get the list of splits for the given Attribute
        member(Range, RangeList),                                                   % loop through every range in the list
        partial_info_gain(Set, Attribute, Range, PartialInfoGain)                   % calculate the current partial info gain
    ),
    % sum all of the partial gains
    findall(PartialInfoGain, PartialGains, GainList),
    sum_list(GainList, PartialGainSum),
    InfoGain is TotalEntropy - PartialGainSum,
    log_i('learner', ['Info gain for ', Attribute, ' is ', InfoGain])
    .

% shortcut for info_gain to the complete set
info_gain(Attribute, InfoGain) :-
    complete_set(CompleteSet),
    info_gain(CompleteSet, Attribute, InfoGain)
    .

/**
 * Calculate a value used to compute the info gain for a given attribute.
 *
 * @param Set           The set of example IDs to calculate the value for.
 * @param Attribute     The attribute name to calculate the value for.
 * @param Range         The specific range(Bottom, Top) for the given Attribute.
 * @return              The PartialInfoGain, to be used to compute the whole Attribute Information Gain.  
 */
partial_info_gain(Set, Attribute, Range, PartialInfoGain) :-
    log_v('learner', ['Calculating partial info gain for ', Attribute, ' with ', Range, '...']),
    clean_set(Set, Attribute, CleanSet),
    % get all the examples that satisfy the given Range
    Subset = (
        example(_, ID, Attribute, Value),
        member(ID, CleanSet),
        is_in_range(Value, Range)
    ),
    % get a list of the IDs
    findall(ID, Subset, SubsetList),
    % get the entropy of the subset
    entropy(SubsetList, SubsetEntropy),
    % get the size of the subset
    length(SubsetList, SubsetLength),
    % get the size of the original set
    length(CleanSet, SetLength),
    PartialInfoGain is (SubsetEntropy * SubsetLength / SetLength),
    log_d('learner', ['Partial info gain for ', Attribute, ' with ', Range, ' is ', PartialInfoGain])
    .

/**
 * Clean the given Set from $null$ values.
 *
 * @param Set           The set of example IDs to clean.
 * @param Attribute     The attribute whose $null$ value must be deleted.
 * @return              The CleanSet, a set whose Attribute does not have $null$ values.
 */
clean_set(Set, Attribute, CleanSet) :-
    CleanSetGoal = (
        % for every member of the original set
        member(ID, Set),
        % get the example
        example(_, ID, Attribute, Value),
        % the value must not be null
        Value \= '$null$'),
    % make a list of it
    findall(ID, CleanSetGoal, CleanSet)
    .

% ------------------ %
%    LEARN PROCESS   %
% ------------------ %
learn :-
    log_e('learner', 'YOU DIDN''T SAY THE MAGIC WORD!').

learn_please :-
    retractall(node(_)),
    assertz(node('root')),
    % TODO: continue here
    true.

/**
 * C4.5
 * Examples:    training examples
 * Target:      target attribute (to be predicted)
 * Attributes:  list of attributes to be tested
 * 
 * If all examples are in class 
 * 
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */
 % TODO: continue here
 c45(Examples, Target, Attributes) :- true.