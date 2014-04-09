/**
 * <module> learner
 *
 * The learner module contains all clauses used for starting the learning
 * process, once all of the examples and symptoms have been asserted by the
 * database.pl module and all classes have been extracted by the categories.pl
 * module.
 *
 * @author Francesco Pontillo
 * @license Apache License, Version 2.0
 */

% --------------------------------------------------------------------------- %
%                               FOLDING EXAMPLES                              %
% --------------------------------------------------------------------------- %

/**
 * example(Kind, ID, Attribute, Value) is semidet.
 * 
 * Accessory predicate to access any kind of example (both training or testing) by specifying 
 * the name the record was asserted with (negative or positive).
 * 
 * @param Kind              The name the record was asserted with; can be positive or negative.
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute of the example.
 * @param Value             The Value of the example.
 */
example(positive, ID, Attribute, Value) :- positive(ID, Attribute, Value).
example(negative, ID, Attribute, Value) :- negative(ID, Attribute, Value).

/**
 * split_examples(+FoldCount, +TestFold) is det.
 * 
 * Split all examples between train_examples/1 and test_examples/1.
 *
 * @param FoldCount         The number of total fold to split the examples between.
 * @param TestFold          The index of the testing fold at the current step.
 */
split_examples(FoldCount, TestFold) :-
    log_d('split_ex', ['Splitting examples with ', FoldCount, ' folds and test fold = ', TestFold]),

    % if the testing fold group is higher than the fold count, log an error
    TestFold > FoldCount,
    log_e('split_examples', 'The TestFold must be less or equal than the FoldCount.'),
    fail;

    % if the fold parameters check is OK, go on
    % split the positive set into folds
    split_examples_pn(positive, FoldCount, TestFold, PositiveTrainExamples, PositiveTestExamples),
    log_d('split_ex', ['Positive test examples for this run are ', PositiveTestExamples]),
    log_d('split_ex', ['Positive train examples for this run are ', PositiveTrainExamples]),
    % split the negative set into folds
    split_examples_pn(negative, FoldCount, TestFold, NegativeTrainExamples, NegativeTestExamples),
    log_d('split_ex', ['Negative test examples for this run are ', NegativeTestExamples]),
    log_d('split_ex', ['Negative train examples for this run are ', NegativeTrainExamples]),

    % generate the test list
    list_append(PositiveTestExamples, NegativeTestExamples, TestExamples),
    % generate the train list
    list_append(PositiveTrainExamples, NegativeTrainExamples, TrainExamples),

    % reset the testing and training lists
    retractall(test_examples(_)),
    retractall(train_examples(_)),

    assertz(test_examples(TestExamples)),
    assertz(train_examples(TrainExamples))
    .

/**
 * split_examples(+PositiveNegative, +FoldCount, +TestFold, -TrainExamples, -TestExamples) is det.
 * 
 * Split positive or negative examples in two different lists: training and testing, according
 * to the fold number and the current test fold.
 * If FoldCount and TestFold equal to 1, simulate the testing and training set as being the same.
 *
 * @param PositiveNegative  The classification of the example, can be positive or negative.
 * @param FoldCount         The number of total fold to split the examples between.
 * @param TestFold          The index of the testing fold at the current step.
 * @param TrainExamples     The generated training examples list of IDs.
 * @param TestExamples      The generated testing examples list of IDs.
 */
split_examples_pn(PositiveNegative, 1, 1, TrainExamples, TestExamples) :-
    findall(ID, example(PositiveNegative,  ID, 'ID', ID), Examples),
    TrainExamples is Examples,
    TestExamples is Examples,
    % cut so it does not continue
    !.
split_examples_pn(PositiveNegative, FoldCount, TestFold, TrainExamples, TestExamples) :-
    % count all retrieved examples in the positive or negative set
    aggregate_all(count, example(PositiveNegative, ID, 'ID', ID), Count),
    FoldCardinality is ceil(Count / FoldCount),
    % the testing fold will have
    % BottomIndex = (TestFold-1)*FoldCardinality and TopIndex = TestFold*FoldCardinality
    TestBottom is ((TestFold-1)*FoldCardinality),
    TestTop is (TestFold*FoldCardinality),
    % partition to get the testing fold IDs list
    findall(ID, example(PositiveNegative,  ID, 'ID', ID), Examples),

    % find all testing IDs
    findall(ID, (
        % whose index is Index (1 to n)
        member(ID, Examples), index_of(Examples, ID, I), Index is I+1, 
        % and is within the folding test limits
        Index >= TestBottom, Index =< TestTop
    ), TestExamples),

    % find all training IDs
    findall(ID, (
        % that is an example
        member(ID, Examples),
        % but that is NOT in the testing list
        not(member(ID, TestExamples))
    ), TrainExamples)
    .


/**
 * train_examples(-List:list) is semidet.
 * 
 * Return the list of current training examples.
 * 
 * @param List              The list of training examples.
 */

/**
 * train_example(Kind, ID, Attribute, Value) is nondet.
 * 
 * Access any kind of training example by specifying the name the record was asserted with 
 * (negative or positive).
 * 
 * @param Kind              The name the record was asserted with; can be positive or negative.
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute of the example.
 * @param Value             The Value of the example.
 */
train_example(Kind, ID, Attribute, Value) :-
    % select examples in the train_examples list
    example(Kind, ID, Attribute, Value),
    train_examples(TrainExamples),
    member(ID, TrainExamples).

/**
 * test_examples(-List:list) is semidet.
 * 
 * Return the list of current testing examples.
 * 
 * @param List              The list of testing examples.
 */

/**
 * test_example(Kind, ID, Attribute, Value) is nondet.
 * 
 * Access any kind of testing example by specifying the name the record was asserted with 
 * (negative or positive).
 * 
 * @param Kind              The name the record was asserted with; can be positive or negative.
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute of the example.
 * @param Value             The Value of the example.
 */
test_example(Kind, ID, Attribute, Value) :-
    % select examples in the TestExamples list
    test_examples(TestExamples),
    example(Kind, ID, Attribute, Value),
    member(ID, TestExamples).

/**
 * train_positive(?ID, ?Attribute, ?Value) is nondet.
 * 
 * Hold positive training examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see negative/3
 */
train_positive(ID, Attribute, Value) :- train_example(positive, ID, Attribute, Value).

/**
 * train_negative(?ID, ?Attribute, ?Value) is nondet.
 * 
 * Hold negative training examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see positive/3
 */
train_negative(ID, Attribute, Value) :- train_example(negative, ID, Attribute, Value).

/**
 * test_positive(?ID, ?Attribute, ?Value) is nondet.
 * 
 * Hold positive testing examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see negative/3
 */
test_positive(ID, Attribute, Value) :- test_example(positive, ID, Attribute, Value).

/**
 * test_negative(?ID, ?Attribute, ?Value) is nondet.
 * 
 * Hold negative testing examples information.
 *
 * @param ID                The ID of the example.
 * @param Attribute         The Attribute for the cell, can be: 'ID', 'Patient', 'Center',
 *                          'PatientSex', 'PatientRace', 'PatientAge', 'SessionID', 'SessionDate',
 *                          'KTV', 'QB', 'ProgWeightLoss', 'RealWeightLoss', 'DeltaWeight', 
 *                          'ProgDuration', 'RealDuration', 'DeltaDuration', 'SAPStart', 'SAPEnd', 
 *                          'SAPAverage', 'DAPStart', 'DAPEnd', 'DAPAverage', 'DeltaBloodFlow', 
 *                          'DeltaUF', 'SymptomID', 'Score'
 * @param Value             The Value for the cell.
 * @see positive/3
 */
test_negative(ID, Attribute, Value) :- test_example(negative, ID, Attribute, Value).

% --------------------------------------------------------------------------- %
%                                ENTROPY MEASURES                             %
% --------------------------------------------------------------------------- %

/**
 * complete_set(?CompleteSet:list) is semidet.
 * 
 * Get the complete set of training example IDs as a list.
 *
 * @param CompleteSet The list of all the training example IDs.
 */
complete_set(CompleteSet) :-
    findall(ID, train_example(_, ID, 'ID', ID), CompleteSet).

/**
 * entropy(+IncludedValues, -Entropy) is semidet.
 * 
 * Calculate the entropy of a given list of training examples (by IDs).
 * 
 * @param IncludedValues    A list of the example IDs to be included when considering the entropy
 *                          calculus. The resulting examples will be intersected with the positive 
 *                          and negative examples.
 *                          If you don't want to filter anything, pass in all of the example IDs:
 *                          ==
 *                          findall(ID, (train_example(_, ID, 'ID', ID),
 *                          train_example(_, ID, 'DeltaWeight', 1)), IncludedValues).
 *                          ==
 * @param Entropy           The resulting entropy of the examples passed by IDs.
 */
entropy(IncludedValues, Entropy) :- 
    % calculate the number of positive training examples (filtered)
    findall(ID, train_example(positive, ID, 'ID', ID), PositiveList),
    intersection(PositiveList, IncludedValues, PositiveSubset),
    length(PositiveSubset, PositiveCount),

    % calculate the number of negative training examples (filtered)
    findall(ID, train_example(negative, ID, 'ID', ID), NegativeList),
    intersection(NegativeList, IncludedValues, NegativeSubset),
    length(NegativeSubset, NegativeCount),

    % calculate the number of all the examples (filtered)
    findall(ID, train_example(_, ID, 'ID', ID), CompleteSet),
    intersection(CompleteSet, IncludedValues, Subset),
    length(Subset, SubsetCount),

    catch(
        (
            P is PositiveCount / SubsetCount,
            N is NegativeCount / SubsetCount,
            log2(P, LogP),
            log2(N, LogN),
            Entropy is (-P*LogP)+(-N*LogN)
        ),
        % if there's an error calculating the log, assume because P or N are 0, so entropy is 0
        _, Entropy is 0
    )
    .

/**
 * entropy(-Entropy) is semidet.
 * 
 * Calculate the entropy of the whole set of training examples. This is a shortcut clause for:
 * ==
 * findall(ID, train_example(ID,'ID',ID), List), entropy(List, Entropy).
 * ==
 * 
 * @param Entropy           The resulting entropy of the whole set of training examples.
 */
entropy(Entropy) :-
    complete_set(CompleteSet),
    entropy(CompleteSet, Entropy).

/**
 * best_attribute(+Set:list, +Attributes:list, -Attribute) is semidet.
 * 
 * Select the best attribute from the given lists of attributes and examples,
 * using the information gain measure.
 * 
 * @param Set               The list of example IDs to calculate the best attribute for.
 * @param Set               The list of attributes to select the best attribute from.
 * @param Attribute         The best attribute for the given set.
 */
best_attribute(Set, Attributes, Attribute) :-
    measure_time,
    % collect the whole set of possible information gains for the given Set
    GainsSet = (target_class(Target), 
        member(Attribute, Attributes), Attribute \= Target, info_gain(Set, Attribute, InfoGain)),
    % calculate the maximum InfoGain from GainsSet and get the Attribute
    aggregate_all(max(InfoGain, Attribute), GainsSet, BestSet),
    BestSet = max(InfoGain, Attribute),
    log_d('best_attr', [
        'Best info gain is achieved with attribute ', Attribute, ' with a value of ', InfoGain]),
    measure_time(Time), format_ms(Time, TimeString),
    log_v('best_attr', ['Best attribute calculus took ', TimeString, '.'])
    .

/**
 * best_attribute(-Attribute) is semidet.
 * 
 * Select the best attribute from the whole set of attributes and training examples.
 * This is a shortcut clause for:
 * ==
 * findall(ID, train_example(ID,'ID',ID), Examples),
 * findall(Attribute, class(Attribute, _), Attributes),
 * best_attribute(Examples, Attributes, Entropy).
 * ==
 * 
 * @param Attribute         The best attribute for all of the exmmples and attributes.
 */
best_attribute(Attribute) :-
    complete_set(CompleteSet),
    % only consider class with a range list (avoid null values altogether)
    findall(Attribute, class(Attribute, _), Attributes),
    % make the actual call implementation
    best_attribute(CompleteSet, Attributes, Attribute)
    .

/**
 * info_gain(+Set:list, +Attribute, -InfoGain) is semidet.
 * 
 * Calculate the Information Gain for a set of training exampes and a given attribute.
 *
 * @param Set               A list of IDs to be included when considering the info gain calculus.
 * @param Attribute         The attribute to calculate the information gain for.
 * @param InfoGain          The calculated information gain.
 */
info_gain(Set, Attribute, InfoGain) :- 
    % first off, let's calculate the total entropy
    entropy(Set, TotalEntropy),

    % calculate the partial information gain on each split of the Attribute (category or class)
    PartialGains = (
        % get the list of splits for the given Attribute
        class(Attribute, RangeList),
        % loop through every range in the list
        member(Range, RangeList),
        % calculate the current partial info gain
        partial_info_gain(Set, Attribute, Range, PartialInfoGain)
    ),
    % sum all of the partial gains
    aggregate_all(sum(PartialInfoGain), PartialGains, PartialGainSum),

    InfoGain is (TotalEntropy - PartialGainSum),
    log_v('info_gain', ['Info gain for ', Attribute, ' is ', InfoGain])
    .

/**
 * info_gain(+Attribute, -InfoGain) is det.
 * 
 * Calculate the Information Gain for all training examples and one attribute.
 * This is a shortcut clause for:
 * ==
 * findall(ID, train_example(ID,'ID',ID), Examples),
 * info_gain(CompleteSet, Attribute, InfoGain).
 * ==
 *
 * @param Attribute         The attribute to calculate the information gain for.
 * @param InfoGain          The calculated information gain.
 */
% shortcut for info_gain to the complete set
info_gain(Attribute, InfoGain) :-
    complete_set(CompleteSet),
    info_gain(CompleteSet, Attribute, InfoGain)
    .

/**
 * partial_info_gain(+Set:list, +Attribute, +Range, -PartialInfoGain) is semidet.
 * 
 * Calculate a partial value used to compute the info gain for a given attribute.
 *
 * @param Set               The list of example IDs to calculate the value for.
 * @param Attribute         The attribute name to calculate the value for.
 * @param Range             The specific range(Bottom, Top) for the given Attribute.
 * @param PartialInfoGain   The partial value to be used to compute the whole Attribute Info Gain.  
 */
partial_info_gain(Set, Attribute, Range, PartialInfoGain) :-
    clean_set(Set, Attribute, CleanSet),
    % get all the training examples that satisfy the given Range
    Subset = (
        member(ID, CleanSet),
        train_example(_, ID, Attribute, Value),
        is_in_range(Value, Range)
    ),
    % get a list of the IDs
    findall(ID, Subset, SubsetList),
    % get the entropy of the subset
    entropy(SubsetList, SubsetEntropy),
    % get the size of the subset
    length(SubsetList, SubsetLength),
    % get the size of the original set
    length(Set, SetLength),
    (SetLength = 0 -> PartialInfoGain is 0;
    PartialInfoGain is (SubsetEntropy * SubsetLength / SetLength))
    %,log_v('par_info_gain', [
    %    'Partial info gain for ', Attribute, ' with ', Range, ' is ', PartialInfoGain])
    .

/**
 * clean_set(+Set:list, +Attribute, -CleanSet) is det.
 * 
 * Clean the given list of example IDs (doesn't matter if training or testing) from '$null$' values.
 *
 * @param Set               The set of example IDs to clean.
 * @param Attribute         The attribute whose $null$ value must be deleted.
 * @param CleanSet          The CleanSet, a list whose Attribute does not have '$null$'' values.
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

/**
 * partition_examples(+InExamples:list, +Attribute, +Range, -OutExamples) is det.
 * 
 * Partition a list of example IDs by analyzing an attribute in a given range.
 * 
 * @param InExamples        List of example IDs to analyze and filter.
 * @param Attribute         The attribute to filter on.
 * @param Range             The range to filter with.
 * @param OutExamples       List of example IDs to return.
 */
 partition_examples(InExamples, Attribute, Range, OutExamples) :-
    findall(Ex, (
        member(Ex, InExamples),                 % from all input members
        example(_, Ex, Attribute, Value),       % get those whose Value of Attribute
        Value \= '$null$',                      % avoid null values!
        is_in_range(Value, Range)),             % is in the given range
    OutExamples).                               % return as list

% --------------------------------------------------------------------------- %
%                                 LEARN PROCESS                               %
% --------------------------------------------------------------------------- %

/**
 * node(?NodeName, ?ParentNode, ?SplitAttribute, ?SplitRange) is semidet.
 * 
 * Holds the node name information, the parent node, and the splitting attribute and range.
 */
:- dynamic node/4.

/**
 * node_label(?Node, ?Label) is semidet.
 * 
 * Holds the node name information, the parent node, and the splitting attribute and range.
 */
:- dynamic node_label/2.

/**
 * learn_please is det.
 * 
 * Start the learning process:
 *   1. partition the positive/3 data set in 10 folds
 *   2. partition the negative/3 data set in 10 folds
 *   3. for every generated fold:
 *      a. start the learning phase
 *      b. start the testing phase
 */
learn_please :-
    timer_start(learn),

    % delete all the rules
    retractall(is_positive(_, _)),
    % delete all test informations
    retractall(test_step(_, _)),
    retractall(test_final(_, _)),

    % from 1 to 10 folds
    TotalFolds is 10,
    between(1, TotalFolds, Step),
        % learn and test for each fold
        learn(TotalFolds, Step),
        test(Step),
        print_le_tree,
    fail;

    % purge the rules and build a collapsed set
    purge_rules,

    % test and assert the information data
    test,

    % log time information
    timer_stop(learn, Elapsed),

    % print and save the learning information
    print_report(Elapsed),

    % save the log of the execution
    save_log(Elapsed),

    % save the rules to file
    save_rules
    .

/**
 * test_step(?Step, ?List) is nondet.
 * 
 * Holds information about a particular step run.
 * 
 * @param Step              The step the error rate was calculated at.
 * @param List              List containing the following:
 *                            - n(AllNegatives)
 *                            - p(AllPositives)
 *                            - rules(GeneratedRules)
 *                            - tn(TrueNegatives)
 *                            - fn(FalseNegatives)
 *                            - tp(TruePositives)
 *                            - fp(FalsePositives)
 *                            - true_pos_rate(TruePosRate)
 *                            - true_neg_rate(TrueNegRate)
 *                            - false_pos_rate(FalsePosRate)
 *                            - false_neg_rate(FalseNegRate)
 *                            - precision(Precision)
 *                            - recall(Recall)
 * 
 * @see test/1.
 */
:- dynamic test_step/2.

/**
 * test_final(?List) is nondet.
 * 
 * Holds information about the final testing process.
 * 
 * @param List              List containing the following:
 *                            - n(AllNegatives)
 *                            - p(AllPositives)
 *                            - rules(GeneratedRules)
 *                            - tn(TrueNegatives)
 *                            - fn(FalseNegatives)
 *                            - tp(TruePositives)
 *                            - fp(FalsePositives)
 *                            - true_pos_rate(TruePosRate)
 *                            - true_neg_rate(TrueNegRate)
 *                            - false_pos_rate(FalsePosRate)
 *                            - false_neg_rate(FalseNegRate)
 *                            - precision(Precision)
 *                            - recall(Recall)
 * 
 * @see test_step/2.
 * @see test/0.
 */
:- dynamic test_final/1.

/**
 * is_positive(+ID, ?LearningStep) is nondet.
 * 
 * Check if a given example/4 ID is positive according to an optionally provided LearningStep.
 * 
 * @param ID                The ID of the example/3 to check for.
 * @param LearningStep      The step number of the learning process.
 *
 * @see check_condition_list/3
 */
 :- dynamic is_positive/2.

/**
 * check_positive(+ID, ?LearningStep) is semidet.
 * 
 * Semi-deterministic version of is_positive/2. If there is at least one is_positive/2 that
 * satisfies the current ID, succeed; otherwise, fail.
 * 
 * @param ID                The ID of the example/3 to check for.
 * @param LearningStep      The step number of the learning process.
 *
 * @see is_positive/2
 */
 check_positive(ID, LearningStep) :-
    example(_, ID, 'ID', ID),
    is_positive(ID, LearningStep), !.

/**
 * check_final_positive(+ID) is semidet.
 * 
 * Final one-time check for the complete and purged set of rules.
 * If there is at least one final_positive/2 that satisfies the current ID, succeed;
 * otherwise, fail.
 * 
 * @param ID                The ID of the example/3 to check for.
 *
 * @see is_positive/2
 */
 check_final_positive(ID) :-
    example(_, ID, 'ID', ID),
    final_positive(ID, _), !.

/**
 * final_positive(?ID, ?StepList) is nondet.
 * 
 * Check if a given example/4 ID is positive. If the example is positive, StepList will be 
 * instantiated with a list of steps that generated the rule that classifies the example 
 * as positive.
 * 
 * @param ID                The ID of the example/3 to check for.
 * @param StepList          A list of step indexes where the satisfactory rule was asserted in.
 *
 * @see is_positive/2, check_condition_list/3
 */
:- dynamic final_positive/2.

/**
 * purge_rules is det.
 * 
 * From the list of rules generated at each step of the learning process, assert in the Prolog
 * memory a set of the same rules, but without any duplicate.
 * A duplicate rule is considered a is_positive/2 rule with the same body.
 * Rules will be asserted as final_positive/2, where the first argument is the ID of the example 
 * to check for and the second argument is the list of steps where the rule was found (duplicated).
 */
purge_rules :-
    retractall(final_positive(_, _)),
    % for each clause, collect  the body
    bagof(Step, clause(is_positive(ID,Step),Body), Steps),
        % assert a new rule, final_positive
        assertz(final_positive(ID,Steps) :- Body),
    fail;
    aggregate_all(count, clause(final_positive(_,_), _), Rules),
    log_d('purge_rules', ['Purging complete, total number of rules: ', Rules]),
    true.

/**
 * test(+Step) is det.
 *
 * Test all test_positive/3 and test_negative/3 and calculates several useful information.
 * The calculated information is asserted as test_step/2.
 * In the end, it prints a report of the run.
 *
 * @param Step              The learning step, defines the particular rules to test against.
 * @see test_step/2
 */
test(Step) :-
    % TN are negative examples not classified
    aggregate_all(count,(test_negative(ID,'ID',ID), not(check_positive(ID, Step))), TrueNegatives),
    % FN are positive examples not classified
    aggregate_all(count,(test_positive(ID,'ID',ID), not(check_positive(ID, Step))), FalseNegatives),
    % TP are positive examples classified as positives
    aggregate_all(count,(test_positive(ID,'ID',ID), check_positive(ID, Step)), TruePositives),
    % FP are negative examples classified as positives
    aggregate_all(count,(test_negative(ID,'ID',ID), check_positive(ID, Step)), FalsePositives),

    % get all negatives
    aggregate_all(count,test_negative(ID,'ID',ID), AllNegatives),
    % get all positives
    aggregate_all(count,test_positive(ID,'ID',ID), AllPositives),

    % calculate the true positive rate (positives classified / positives)
    (AllPositives = 0 -> TruePosRate is 1; 
        TruePosRate is (TruePositives/AllPositives)),
    % calculate the true negative rate (negatives classified / negatives)
    (AllNegatives = 0 -> TrueNegRate is 1; 
        TrueNegRate is (TrueNegatives/AllNegatives)),
    % calculate the false positive rate (negatives classified as positives / negatives)
    (AllNegatives = 0 -> FalsePosRate is 0; 
        FalsePosRate is (FalsePositives/AllNegatives)),
    % calculate the false negative rate (positives not classified / positives)
    (AllPositives = 0 -> FalseNegRate is 0; 
        FalseNegRate is (FalseNegatives/AllPositives)),

    % calculate the recall, rate of real positive examples recalled (recall = tp/(tp+fn))
    (TestRecall is (TruePositives + FalseNegatives), TestRecall = 0 -> Recall is 0; 
        Recall is (TruePositives/(TruePositives + FalseNegatives))),
    % calculate the precision, rate of real positive examples among all positives (pr = tp/(tp+fp))
    (TestPrecision is (TruePositives + FalsePositives), TestPrecision = 0 -> Precision is 0; 
        Precision is (TruePositives/(TruePositives + FalsePositives))),

    % calculate the F-measure
    (TestFMeasure is (Precision+Recall), TestFMeasure = 0 -> FMeasure is 0; 
        FMeasure is (2*Precision*Recall/(Precision+Recall))),

    % count the generated rules
    aggregate_all(count,clause(is_positive(_, Step), _), GeneratedRules),

    % save the new testing information
    assertz(test_step(Step, [
        n(AllNegatives),
        p(AllPositives),
        rules(GeneratedRules),
        tp_rate(TruePosRate),
        tn_rate(TrueNegRate),
        fp_rate(FalsePosRate),
        fn_rate(FalseNegRate),
        tn(TrueNegatives),
        fn(FalseNegatives), 
        tp(TruePositives),
        fp(FalsePositives),
        precision(Precision),
        recall(Recall),
        f_measure(FMeasure)
    ])),

    log_i('test', ['STEP:          ', Step]),
    log_i('test', ['  - TP:        ', TruePositives]),
    log_i('test', ['  - TN:        ', TrueNegatives]),
    log_i('test', ['  - FN:        ', FalseNegatives]),
    log_i('test', ['  - FP:        ', FalsePositives]),
    log_i('test', ['  - TP Rate:   ', TruePosRate]),
    log_i('test', ['  - TN Rate:   ', TrueNegRate]),
    log_i('test', ['  - FP Rate:   ', TruePosRate]),
    log_i('test', ['  - FN Rate:   ', FalseNegRate]),
    log_i('test', ['  - Precision: ', Precision]),
    log_i('test', ['  - Recall:    ', Recall]),
    log_i('test', ['  - F-Measure: ', FMeasure]),
    log_i('test', ['  - RULES:     ', GeneratedRules])
    .
/**
 * test is det.
 *
 * Test all positive/3 and negative/3 and calculates several useful information.
 * The calculated information is asserted as test_final/1.
 *
 * @see test_final/1
 */
test :-
    % TN are negative examples not classified
    aggregate_all(count, (negative(ID,'ID',ID), not(check_final_positive(ID))), TrueNegatives),
    % FN are positive examples not classified
    aggregate_all(count, (positive(ID,'ID',ID), not(check_final_positive(ID))), FalseNegatives),
    % TP are positive examples classified as positives
    aggregate_all(count, (positive(ID,'ID',ID), check_final_positive(ID)), TruePositives),
    % FP are negative examples classified as positives
    aggregate_all(count, (negative(ID,'ID',ID), check_final_positive(ID)), FalsePositives),

    % get all negatives
    aggregate_all(count, negative(ID,'ID',ID), AllNegatives),
    % get all positives
    aggregate_all(count, positive(ID,'ID',ID), AllPositives),

    % calculate the true positive rate (positives classified / positives)
    (AllPositives = 0 -> TruePosRate is 1; 
        TruePosRate is (TruePositives/AllPositives)),
    % calculate the true negative rate (negatives classified / negatives)
    (AllNegatives = 0 -> TrueNegRate is 1; 
        TrueNegRate is (TrueNegatives/AllNegatives)),
    % calculate the false positive rate (negatives classified as positives / negatives)
    (AllNegatives = 0 -> FalsePosRate is 0; 
        FalsePosRate is (FalsePositives/AllNegatives)),
    % calculate the false negative rate (positives not classified / positives)
    (AllPositives = 0 -> FalseNegRate is 0; 
        FalseNegRate is (FalseNegatives/AllPositives)),

    % calculate the recall, rate of real positive examples recalled (recall = tp/(tp+fn))
    (TestRecall is (TruePositives + FalseNegatives), TestRecall = 0 -> Recall is 0; 
        Recall is (TruePositives/(TruePositives + FalseNegatives))),
    % calculate the precision, rate of real positive examples among all positives (pr = tp/(tp+fp))
    (TestPrecision is (TruePositives + FalsePositives), TestPrecision = 0 -> Precision is 0; 
        Precision is (TruePositives/(TruePositives + FalsePositives))),

    % calculate the F-measure
    (TestFMeasure is (Precision+Recall), TestFMeasure = 0 -> FMeasure is 0; 
        FMeasure is (2*Precision*Recall/(Precision+Recall))),

    % count the generated rules
    aggregate_all(count, clause(final_positive(_, _), _), GeneratedRules),

    % save the new testing information
    retractall(test_final(_)),
    assertz(test_final([
        n(AllNegatives),
        p(AllPositives),
        rules(GeneratedRules),
        tp_rate(TruePosRate),
        tn_rate(TrueNegRate),
        fp_rate(FalsePosRate),
        fn_rate(FalseNegRate),
        tn(TrueNegatives),
        fn(FalseNegatives), 
        tp(TruePositives),
        fp(FalsePositives),
        precision(Precision),
        recall(Recall),
        f_measure(FMeasure)
    ]))
    .

/**
 * learn(+Step) is det.
 *
 * Start the learning process using given sets of training and testing examples by adding 
 * node/4 and node_label/2 clauses to the database.
 * 
 * @param TotalFolds        The number of folds to split the examples into.
 * @param Step              The current learning step.
 */
learn(TotalFolds, Step) :-
    % the testing fold is the last fold for the first step and the first fold for the last step
    TestFold is TotalFolds-Step+1,
    split_examples(TotalFolds, TestFold),
    % clear old facts
    retractall(node(_,_,_,_)), retractall(node_label(_,_)),
    % get a list of every valid attribute (the ones with at least one range)
    findall(Attribute, class(Attribute, _), Attributes),
    % get a list of every training example
    complete_set(Examples),
    % create the root node
    log_d('learn', ['Create root node for step', Step, '.']),
    RootNode = node('root', root, _Root, root),
    assertz(RootNode),
    % bootstrap the algorithm with the root node
    log_i('learn', ['Bootstrapping C4.5 for step ', Step, '...']),
    c45(RootNode, Examples, Attributes),
    gen_all_the_rulez(Step)
    .

/**
 * c45(+Node, +Examples:list, +Attributes:list) is det.
 * 
 * Apply the C4.5 algorithm to a given node, that will be split according to the training examples 
 * passed and the available attributes still left.
 *
 * @param Node              The node to build.
 * @param Examples          The training examples.
 * @param Attributes        The list of attributes to be tested.
 */

 c45(Node, Examples, Attributes) :- 
    Node = node(NodeName, _, Attribute, _),
    log_v('c45', ['Executing C4.5 for node ', NodeName, '.']),

    target_class(TargetAttr),

    % get all the current target values
    findall(Value,  (
        member(ID, Examples),
        train_example(_, ID, Attribute, NotNullValue),
        NotNullValue \= '$null$', 
        train_example(_, ID, TargetAttr, Value)), 
    TargetValues),

    % STEP 1: check if every training example is in the same class
    % find all the different target values in the current subset, after cleaning it from null values
    log_v('c45', ['Current examples are : ', Examples]),
    % if all current examples are in one class only (p or n) the node has a label (yay!)
    setof(TheID, member(TheID, TargetValues), DifferentTargetsList),
    log_v('c45-test', ['Non null different training targets left: ', DifferentTargetsList]),
    length(DifferentTargetsList, DifferentValues),
    (
        DifferentValues = 1 ->
        TargetValues = [SingleValue|_],
        log_d('c45', ['All examples are in the same class: ', SingleValue, '.']),
        assertz(node_label(Node, SingleValue)),
        fail;
        true
    ),

    % STEP 2: check for empty attribute list
    length(Attributes, AttributesNumber),
    % if the attribute list is empty, node_label must hold the most common target value
    (
        AttributesNumber = 0 ->
        log_d('c45', ['There are no more attributes to analyze.']),
        list_most_common(TargetValues, MostCommonValue, _),
        assertz(node_label(Node, MostCommonValue)),
        fail;
        true
    ),

    % RECURSION STEP: split the training examples according to the best attribute
    log_d('c45', ['Looking for the best attribute to split ', NodeName]),
    % get the best attribute
    best_attribute(Examples, Attributes, BestAttribute),
    % save the remaining attributes for later use
    list_remove(Attributes, BestAttribute, RemainingAttributes),
    % get all the ranges of the best attribute
    class(BestAttribute, BestAttributeRanges),
    (
        % for each range of the BestAttribute
        member(Range, BestAttributeRanges),
            log_v('c45', ['Looping over range ', Range]),
            % get a node name in the '[ AttributeName : Range ]' format
            term_to_atom(Range, RangeString),
            atomic_list_concat(['[ ', BestAttribute, ' : ', RangeString, ' ]'], ChildNodeName),
            % create the new node with the parent node, the splitting attribute and range
            log_d('c45', ['Create node ', ChildNodeName, '.']),
            ChildNode = node(ChildNodeName, Node, BestAttribute, Range),
            assertz(ChildNode),
            % partition the examples on the given best attribute and range
            partition_examples(Examples, BestAttribute, Range, NewExamples),
            % if the new example set is empty
            length(NewExamples, NewExamplesLength),
            (
                NewExamplesLength = 0 ->
                log_d('c45', [
                    'There are no more examples to analyze for attribute ', 
                    BestAttribute, ' in range ', Range, '.']
                ),
                list_most_common(TargetValues, MostCommonValue, _),
                assertz(node_label(ChildNode, MostCommonValue))
                ;
                c45(ChildNode, NewExamples, RemainingAttributes)
            ),
        % loop until all ranges are analyzed and nodes are created
        fail
    ).
% always succeed
c45(_,_,_) :- true.

/**
 * print_le_tree is semidet.
 *
 * Print the learnt tree, if there is a `node('root', root, Root, root)`.
 */
print_le_tree :-
    RootNode = node('root', root, _Root, root),
    print_le_branch(RootNode, 0).

/**
 * print_le_branch(+Node, +NestLevel) is semidet.
 *
 * Print a given node with a nest level that decides how much left space
 * the node representation must have.
 *
 * A node will be printed with:
 *   - a check character, if the node is terminal and classifies positive examples
 *   - a uncheck character, if the node is terminal and doesn't classify positive examples
 *   - a down arrow, if the node is not terminal
 *
 * @param Node              The node to print.
 * @param NestLevel         The nesting level to be used.
 */
print_le_branch(Node, NestLevel) :-
    % print 4 spaces for each level
    between(0, NestLevel, Current), Current > 0, write('    '), fail;

    % if it is a leaf node, print the result
    node_label(Node, Result),
    positive_target(Pos),
    (
        Result = Pos -> 
            string_codes(Check, [32, 10004, 32]), Opts = [bold, bg(green), fg(white)];
            string_codes(Check, [32, 10008, 32]), Opts = [bold]
    ),
    ansi_format(Opts, Check, []),
    fail;

    % if it's not a leaf node, print a down triangle to indent properly
    not(node_label(Node, _)), NestLevel > 0,
    atom_codes(Triangle, [32, 9660, 32]), write(Triangle),
    fail;

    % print the attribute and the range]), 
    Node = node(_, _, Attribute, Range),
    write('[ '),
    write(Attribute),
    write(' : '),
    write(Range),
    write(' ]'), fail;

    % always go to new line
    nl, fail;

    % loop through the children and print them
    node(ChildNodeName, Node, Attribute, Range),
    ChildNode = node(ChildNodeName, Node, Attribute, Range),
    NextNest is NestLevel + 1,
    print_le_branch(ChildNode, NextNest), fail;

    % always succeed
    true.

% --------------------------------------------------------------------------- %
%                                 RULE GENERATOR                              %
% --------------------------------------------------------------------------- %

/**
 * gen_all_the_rulez(+Step) is semidet.
 * 
 * For each node_label/2 with a positive outcome, generate the corresponding rule
 * by going up from the leaf to the tree root node.
 * The rule will be is_positive/2.
 * 
 * @param Step              The current learning step.
 */
gen_all_the_rulez(Step) :- 
    log_d('gen_all_rules', ['Starting to generate rules for step ', Step, '...']),
    positive_target(PositiveID),                % get the positive ID
    node_label(Node, PositiveID),               % for every positive node
    gen_rule(Node, Step),                       % generate corresponding rule
    fail                                        % loop
    ;
    log_i('gen_all_rules', ['Rules generation complete for step ', Step, '.']),
    true                                        % always succeed
    .

/**
 * gen_rule(+Node, +Step) is semidet.
 * 
 * Generate the rule for the corresponding input Node.
 *
 * @param Node              The leaf Node that holds the rule information.
 * @param Step              The current learning step.
 */
gen_rule(Node, Step) :-
    Node = node(Name, _, _, _),
    log_v('gen_rule', ['Generating rule for node ', Name, '...']),
    get_rule_list(Node, [], Conditions),
    assertz(is_positive(ID, Step) :- (check_condition_list(ID, Conditions))),
    log_v('gen_rule', ['Generated rule for node ', Name, ' at step ', Step, '.'])
    .

/**
 * condition(?Attribute, ?Range) is semidet.
 *
 * Holds information about a condition of success for an Attribute in a given Range.
 *
 * @param Attribute         The attribute to test the condition onto.
 * @param Range             The range to be used for the test.
 */

/**
 * get_rule_list(+Node, +PrevList:list, -List:list) is semidet.
 * 
 * Builds a list of `condition(Attribute, Range)` given a node, concatenating
 * the conditions to the given PrevList (can be empty).
 * 
 * @param Node              The Node to build the condition list for.
 * @param PrevList          The temporary list for recursion.
 * @param List              The list of conditions to return.
 */
get_rule_list(Node, PrevList, List) :-
    Node = node(root, root, _Root, root),
    List = PrevList, !
    ;
    Node = node(_, Parent, Attribute, Range),
    Element = condition(Attribute, Range),
    list_append(PrevList, Element, NewPrevList),
    get_rule_list(Parent, NewPrevList, List)
    .

/**
 * ensure_not_null_conditions(+ID, +List:list) is semidet.
 * 
 * Succeed if:
 *   - the List is empty
 *   - there is at least one condition Attribute that example/4 with the given ID has not null
 * Otherwise, it fails.
 *
 * @param ID                The ID of the example/4 to check.
 * @param List              The list of condition/2 to loop through.
 */
% for empty list, always succeed
ensure_not_null_conditions(_, []).
% if here, the list has 1 element only
ensure_not_null_conditions(ID, [condition(Attribute, _)]) :-
    % if the Attribute is found, the value must not be null
    example(_, ID, Attribute, Value), !,
    Value \= '$null$', !.
% if here, Tail contains at least 1 element
ensure_not_null_conditions(ID, [condition(Attribute, _) | Tail]) :-
    ensure_not_null_conditions(ID, [condition(Attribute, _)]), !
    ;
    ensure_not_null_conditions(ID, Tail), !.

/**
 * check_condition_list(+ID, +List:list) is semidet.
 * 
 * Check if a given example with an ID matches all the conditions in the input list.
 * 
 * @param ID                The ID of the example.
 * @param List              The List of condition/2.
 */
check_condition_list(ID, List) :-
    % make sure the example with the given ID exists
    example(_, ID, 'ID', ID), !,
    ensure_not_null_conditions(ID, List),
    forall(
        member(Condition, List),
        (
            % get Attribute and Range to test
            Condition = condition(Attribute, Range),
            % test the Attribute and the Range
            example(_, ID, Attribute, Value),
            % ignore $null$ values
            (Value \= '$null$' -> 
                is_in_range(Value, Range),
                log_v('check', [
                    'Test OK: ID ', ID, ', Attribute ', Attribute, ', value ', Value, ', ', Range, '.']
                );
                true
            )
        )
    )
    .

/**
 * get_conditions_from_list(+Conditions:list, -Condition) is det.
 * 
 * Generate a set of Prolog conjunctives from a list of condition/2.
 * 
 * @param List              The list of condition/2.
 * @param Set               The set of Prolog conjunctives.
 */
get_conditions_from_list([Condition], Condition).
get_conditions_from_list([Head | Tail],(Head, OtherConditions)) :-
    get_conditions_from_list(Tail, OtherConditions).

/**
 * print_report(+Elapsed) is det.
 * 
 * Print a detailed report of the learning algorithm and of the executed tests.
 *
 * @param Elapsed           The number of seconds the algorithm has taken to complete
 */
print_report(Elapsed) :- 
    positive_target(PositiveID), 
    format_s(Elapsed, Time),
    log_i('report', ['Learning algorithm finished in ', Time, '.']),
    log_i('report', ['Symptom: ', PositiveID]),

    aggregate_all(count, test_step(_,_), Runs),

    aggregate_all(count, example(positive, ID, 'ID', ID), PositiveCount),
    aggregate_all(count, example(negative, ID, 'ID', ID), NegativeCount),

    log_i('report', ['Positive examples: ', PositiveCount]),
    log_i('report', ['Negative examples: ', NegativeCount]),
    log_i('report', ['Total runs: ', Runs]),

    log_i('report', 'Runs recap:'),
    forall(between(1, Runs, Run), 
        (
        test_step(Run, RunData),
        member(tp_rate(TruePosRate), RunData),
        member(fp_rate(FalsePosRate), RunData),
        member(precision(Precision), RunData),
        member(recall(Recall), RunData),
        member(f_measure(FMeasure), RunData),
        member(rules(GeneratedRules), RunData),
        % print results to screen
        log_i('report', [
            '  - Run ', Run, 
            ' | Rules : ', GeneratedRules,
            ' | TP Rate : ', TruePosRate, 
            ' | FP Rate : ', FalsePosRate, 
            ' | Precision : ', Precision, 
            ' | Recall : ', Recall, 
            ' | F-Measure : ', FMeasure
            ])
        )
    ),

    aggregate_all(sum(TruePosRate), (test_step(_, Data), member(tp_rate(TruePosRate), Data)),
        TruePosRateSum),
    aggregate_all(sum(FalsePosRate), (test_step(_, Data), member(fp_rate(FalsePosRate), Data)), 
        FalsePosRateSum),
    aggregate_all(sum(Precision), (test_step(_, Data), member(precision(Precision), Data)), 
        PrecisionSum),
    aggregate_all(sum(Recall), (test_step(_, Data), member(recall(Recall), Data)), 
        RecallSum),
    aggregate_all(sum(FMeasure), (test_step(_, Data), member(f_measure(FMeasure), Data)), 
        FMeasureSum),

    AverageTruePosRate is TruePosRateSum/Runs,
    AverageFalsePosRate is FalsePosRateSum/Runs,
    AveragePrecision is PrecisionSum/Runs,
    AverageRecall is RecallSum/Runs,
    AverageFMeasure is FMeasureSum/Runs,

    log_i('report', ['AVERAGES:']),
    log_i('report', ['  - TP Rate:   ', AverageTruePosRate]),
    log_i('report', ['  - FP Rate:   ', AverageFalsePosRate]),
    log_i('report', ['  - Precision: ', AveragePrecision]),
    log_i('report', ['  - Recall:    ', AverageRecall]),
    log_i('report', ['  - F-Measure: ', AverageFMeasure]),

    % save the new testing information
    test_final(Data),
    member(tp(TruePositives), Data),
    member(tn(TrueNegatives), Data),
    member(fp(FalsePositives), Data),
    member(fn(FalseNegatives), Data),
    member(tp_rate(TruePosRate), Data),
    member(tn_rate(TrueNegRate), Data),
    member(fp_rate(FalsePosRate), Data),
    member(fn_rate(FalseNegRate), Data),
    member(precision(Precision), Data),
    member(recall(Recall), Data),
    member(f_measure(FMeasure), Data),
    member(rules(GeneratedRules), Data),

    log_i('test', ['FINAL:         ']),
    log_i('test', ['  - TP:        ', TruePositives]),
    log_i('test', ['  - TN:        ', TrueNegatives]),
    log_i('test', ['  - FP:        ', FalsePositives]),
    log_i('test', ['  - FN:        ', FalseNegatives]),
    log_i('test', ['  - TP Rate:   ', TruePosRate]),
    log_i('test', ['  - TN Rate:   ', TrueNegRate]),
    log_i('test', ['  - FP Rate:   ', FalsePosRate]),
    log_i('test', ['  - FN Rate:   ', FalseNegRate]),
    log_i('test', ['  - Precision: ', Precision]),
    log_i('test', ['  - Recall:    ', Recall]),
    log_i('test', ['  - F-Measure: ', FMeasure]),
    log_i('test', ['  - Rules:     ', GeneratedRules]),

    log_d('report', 'Report printed.').

/**
 * save_log(+Elapsed) is det.
 * 
 * Save a log .csv file: 'runs/log_{ID}.csv'.
 * The 'runs' folder must exist.
 * 
 * The file will list, for each run:
 *   - the symptom ID (always the same for each file)
 *   - the run index
 *   - the number of generated rules
 *   - the true positive rate
 *   - the false positive rate
 *   - the precision
 *   - the recall
 *   - the F-Measure
 * 
 * The last line contains:
 *   - the time of total execution (in seconds)
 *   - the number of positive examples (including both training and testing examples)
 *   - the final number of generated rules
 *   - the final true positive rate
 *   - the final false positive rate
 *   - the final precision
 *   - the final recall
 *   - the final F-Measure
 * 
 * @param Elapsed           Total time of execution, in seconds.
 */
save_log(Elapsed) :-
    % get the positive ID
    positive_target(PositiveID), 
    % get the number of total runs
    aggregate_all(count, test_step(_, _), Runs),

    % open a csv file to log the results
    atom_string(PositiveID, SymptomString),
    concat_string_list(['runs/log_', SymptomString, '.csv'], PathCSV),
    open(PathCSV, write, Log),

    write(Log, '"Symptom";"Run";"Rules";"TP Rate";"FP Rate";"Precision";"Recall";"F-Measure";\n'),

    % for every run
    forall(between(1, Runs, Run), (
        % get the run information
        test_step(Run, RunData),
        member(rules(GeneratedRules), RunData),
        member(tp_rate(TruePosRate), RunData),
        member(fp_rate(FalsePosRate), RunData),
        member(precision(Precision), RunData),
        member(recall(Recall), RunData),
        member(f_measure(FMeasure), RunData),
        % log the run to the csv
        write(Log, SymptomString), write(Log, ';'),
        write(Log, Run), write(Log, ';'),
        write(Log, GeneratedRules), write(Log, ';'),
        write(Log, TruePosRate), write(Log, ';'),
        write(Log, FalsePosRate), write(Log, ';'),
        write(Log, Precision), write(Log, ';'),
        write(Log, Recall), write(Log, ';'),
        write(Log, FMeasure), write(Log, ';'),
        write(Log, '\n')
    )),

    % separator
    write(Log, '"";"";"";"";\n'),
    write(Log, 
        '"Time (s)";"Positives";"Rules";"TP Rate";"FP Rate";"Precision";"Recall";"F-Measure";\n'),

    % grab the data from test_final/1
    test_final(Data),
    member(p(FinalPositives), Data),
    member(rules(FinalRules), Data),
    member(tp_rate(FinalTruePosRate), Data),
    member(fp_rate(FinalFalsePosRate), Data),
    member(precision(FinalPrecision), Data),
    member(recall(FinalRecall), Data),
    member(f_measure(FinalFMeasure), Data),

    % log final data
    write(Log, Elapsed), write(Log, ';'),
    write(Log, FinalPositives), write(Log, ';'),
    write(Log, FinalRules), write(Log, ';'),

    write(Log, FinalTruePosRate), write(Log, ';'),
    write(Log, FinalFalsePosRate), write(Log, ';'),
    write(Log, FinalPrecision), write(Log, ';'),
    write(Log, FinalRecall), write(Log, ';'),
    write(Log, FinalFMeasure), write(Log, ';'),

    % close the csv
    write(Log, '\n'),
    close(Log),

    log_d('save_log', 'Tests written to file.')
    .

/**
 * save_rules is det.
 * 
 * Save all generated rules in the prolog file: 'runs/rules_{ID}.pl'.
 * The 'runs' folder must exist.
 */
save_rules :-
    % get the positive ID
    positive_target(PositiveID), 
    % open a pl file to log the results
    atom_string(PositiveID, SymptomString),
    concat_string_list(['runs/rules_', SymptomString, '.pl'], PathPL),
    open(PathPL, write, RulesFile),
    % write the generated rules
    with_output_to(RulesFile, listing(final_positive(_, _))),
    close(RulesFile),

    log_d('save_rules', 'Rules written to file.')
    .
