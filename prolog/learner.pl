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

split_examples(FoldCount, TestFold) :-
    log_d('split_ex', ['Splitting examples with ', FoldCount, ' folds and test fold = ', TestFold]),

    % if the testing fold group is higher than the fold count, log an error
    TestFold > FoldCount,
    log_e('split_examples', 'The TestFold must be less or equal than the FoldCount.'),
    fail;

    % if the fold parameters check is OK, go on
    % count all retrieved examples
    count_all_examples(Count),
    FoldCardinality is ceil(Count / FoldCount),
    % the testing fold will have
    % BottomIndex = (TestFold-1)*FoldCardinality and TopIndex = TestFold*FoldCardinality
    TestBottom is ((TestFold-1)*FoldCardinality),
    TestTop is (TestFold*FoldCardinality),

    log_v('split_ex', ['The test fold has index range of [', TestBottom, ',', TestTop, ').']),

    % partition to get the testing fold IDs list
    findall(ID, example(_,ID,'ID',ID), Examples),

    % find all testing IDs
    findall(ID, (
        % whose index is Index (1 to n)
        member(ID, Examples), index_of(Examples, ID, I), Index is I+1, 
        % and is within the folding test limits
        Index >= TestBottom, Index < TestTop
    ), TestExamples),

    % find all training IDs
    findall(ID, (
        % that is an example
        member(ID, Examples),
        % but that is NOT in the testing list
        not(member(ID, TestExamples))
    ), TrainExamples),

    % reset the testing and training lists
    retractall(test_examples(_)),
    retractall(train_examples(_)),

    assertz(test_examples(TestExamples)),
    assertz(train_examples(TrainExamples)),

    log_v('split_ex', ['Test examples for this run are ', TestExamples]),
    log_v('split_ex', ['Train examples for this run are ', TrainExamples])
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
    log_d('best_attr', ['Best attribute calculus took ', TimeString, '.'])
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
    log_v('info_gain', ['Calculating info gain for ', Attribute, '...']),
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
    findall(PartialInfoGain, PartialGains, GainList),
    sum_list(GainList, PartialGainSum),
    InfoGain is TotalEntropy - PartialGainSum,
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
    length(CleanSet, SetLength),
    PartialInfoGain is (SubsetEntropy * SubsetLength / SetLength),
    log_v('par_info_gain', [
        'Partial info gain for ', Attribute, ' with ', Range, ' is ', PartialInfoGain])
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
 * TODO: customize number of folds?
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
    % delete all error rate calculations
    retractall(test_error_rate(_, _)),

    % from 1 to 10 folds
    TotalFolds is 10,
    between(1, TotalFolds, Step),
        % learn and test for each fold
        learn(TotalFolds, Step),
        test(Step),
    fail;

    % log time information
    timer_stop(learn, Elapsed), format_s(Elapsed, Time),
    log_i('learn', ['Learning algorithm finished in ', Time])
    .

/**
 * test_error_rate(?Step, ?ErrorRate) is nondet.
 * 
 * Holds information about the error rate calculated at a particular Step.
 * 
 * @see test/1.
 */
:- dynamic test_error_rate/2.

/**
 * test(+Step) is det.
 *
 * Test all test_positive/3 and test_negative/3 and calculates an error rate as wrong predictions
 * on the total number of predictions (single tests) executed.
 * The calculated error rate is asserted as test_error_rate/2.
 *
 * @param Step              The learning step, defines the particular rules to test against.
 * @see test_error_rate/2
 */
test(Step) :-
    aggregate_all(count, (test_negative(ID,'ID',ID), is_positive(ID, Step)), FalsePositives),
    aggregate_all(count, (test_positive(ID,'ID',ID), not(is_positive(ID, Step))), FalseNegatives),
    test_examples(TestExamples), length(TestExamples, Tests),
    (
        Tests = 0 -> ErrorRate is 0; ErrorRate is ((FalsePositives+FalseNegatives)/Tests)
    ),
    assertz(test_error_rate(Step, ErrorRate)),
    log_i('test', ['Error Rate at step ', Step, ' = ', ErrorRate, '.'])
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
    log_d('learn', 'Create root node.'),
    RootNode = node('root', root, root, root),
    assertz(RootNode),
    % bootstrap the algorithm with the root node
    log_i('learn', 'Bootstrap C4.5'),
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
    Node = node(NodeName, _, _, _),
    log_d('c45', ['Executing C4.5 for node ', NodeName, '.']),
    % STEP 1: check if every training example is in the same class
    % find all the different target values in the current subset
    target_class(TargetAttr),
    findall(Value,  (member(ID, Examples), train_example(_, ID, TargetAttr, Value)), TargetValues),
    % if all current examples are in one class only (p or n) the node has a label (yay!)
    % TODO: check the percentage and apply a threshold
    length(TargetValues, DifferentValues),
    (
        DifferentValues = 1 ->
        TargetValues = [SingleValue|_],
        log_d('c45', ['All examples are in the same class: ', SingleValue, '.']),
        assertz(node_label(Node, SingleValue)),
        fail
        ;
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
        fail
        ;
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
                log_d('c45', ['Recursively calling C4.5 on ', ChildNodeName, '.']),
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
 * Print the learnt tree, if there is a `node('root', root, root, root)`.
 */
print_le_tree :-
    RootNode = node('root', root, root, root),
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
 * is_positive(+ID, ?LearningStep) is nondet.
 * 
 * Check if a given example/4 ID is positive according to an optionally provided LearningStep.
 * 
 * @param ID                The ID of the fact/3 to check for.
 * @param LearningStep      The step number of the learning process.
 *
 * @see check_condition_list/3
 */

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
    Node = node(root, root, root, root),
    List = PrevList, !
    ;
    Node = node(_, Parent, Attribute, Range),
    Element = condition(Attribute, Range),
    list_append(PrevList, Element, NewPrevList),
    get_rule_list(Parent, NewPrevList, List)
    .

/**
 * check_condition_list(+ID, +List) is semidet.
 * 
 * Check if a given example with an ID matches all the conditions in the input list.
 * 
 * @param ID                The ID of the example.
 * @param List              The List of condition/2.
 */
check_condition_list(ID, List) :-
    example(_, ID, _, _),
    forall(
        member(Condition, List),
        (
            % get Attribute and Range to test
            Condition = condition(Attribute, Range),
            % test the Attribute and the Range
            example(_, ID, Attribute, Value),
            log_v('check', [
                'Testing ID ', ID, ' on Attribute ', Attribute, 
                ' with value ', Value, ' on ', Range, '.']
            ),
            is_in_range(Value, Range)
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
 * print_report is det.
 *
 * Print a detailed report of the learning algorithm and of the executed tests.
 */
print_report :- 
    aggregate_all(sum(SingleError), test_error_rate(_, SingleError), ErrorSum),
    aggregate_all(count, test_error_rate(_, _), Runs),
    TotalError is ErrorSum/Runs,

    log_i('report', ['Total runs: ', Runs]),

    log_i('report', 'Runs error rates:'),
    between(1, Runs, Run),
        test_error_rate(Run, ErrorRate),
        log_i('report', ['    - Run ', Run, ' : Error ', ErrorRate]),
    fail;

    log_i('report', ['TOTAL ERROR: ', TotalError])
    .
