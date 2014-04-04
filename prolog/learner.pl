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
    log_d('best_attribute', ['Best info gain is achieved with attribute ', Attribute, ' with a value of ', InfoGain]),
    measure_time(Time), format_ms(Time, TimeString),
    log_d('best_attribute', ['Best attribute calculus took ', TimeString, '.'])
    .

/**
 * TODO: write doc
 */
% shortcut for best_attribute to the complete set of examples and attributes (only the one with ranges)
best_attribute(Attribute) :-
    complete_set(CompleteSet),
    findall(Attribute, class(Attribute, _), Attributes),        % only consider class with a range list (avoid null values altogether)
    best_attribute(CompleteSet, Attributes, Attribute)          % make the actual call implementation
    .

/**
 * Calculate the Information Gain for a set of values and a given attribute.
 *
 * @param Set                   A list of IDs to be included when considering the information gain calculus.
 * @param Attribute             The attribute to calculate the information gain for.
 * @Return InfoGain             The calculated information gain.
 */
info_gain(Set, Attribute, InfoGain) :- 
    log_v('info_gain', ['Calculating info gain for ', Attribute, '...']),
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
    log_v('info_gain', ['Info gain for ', Attribute, ' is ', InfoGain])
    .

/**
 * TODO: write doc
 */
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
    log_v('partial_info_gain', ['Calculating partial info gain for ', Attribute, ' with ', Range, '...']),
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
    log_v('partial_info_gain', ['Partial info gain for ', Attribute, ' with ', Range, ' is ', PartialInfoGain])
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

/**
 * Partition a set of examples by analyzing an attribute in a given range.
 * 
 * @param InExamples    List of example IDs to analyze and filter
 * @param Attribute     The attribute to filter on
 * @param Range         The range to filter with
 * @param OutExamples   List of example IDs to return
 */
 partition_examples(InExamples, Attribute, Range, OutExamples) :-
    findall(Ex, (
        member(Ex, InExamples),                 % from all input members
        example(_, Ex, Attribute, Value),       % get those whose Value of Attribute
        Value \= '$null$',                      % avoid null values!
        is_in_range(Value, Range)),             % is in the given range
    OutExamples).                               % return as list

% ------------------ %
%    LEARN PROCESS   %
% ------------------ %

% node/4 holds the node, the parent node, the splitting attribute and range
:- dynamic node/4.
% node_label/2 holds the node result
:- dynamic node_label/2.

/**
 * Bootstrap the learning process on the root node.
 */
learn_please :-
    % clear old facts
    retractall(node(_,_,_,_)), retractall(node_label(_,_)),
    % get a list of every valid attribute (the ones with at least one range)
    findall(Attribute, class(Attribute, _), Attributes),
    % get a list of every example
    complete_set(Examples),
    % create the root node
    log_d('learn', 'Create root node.'),
    RootNode = node('root', root, root, root),
    assertz(RootNode),
    % bootstrap the algorithm with the root node
    log_i('learn', 'Bootstrap C4.5'),
    timer_start(learn),
    c45(RootNode, Examples, Attributes),
    timer_stop(learn, Elapsed), format_s(Elapsed, Time),
    log_i('learn', ['Learning algorithm finished in ', Time])
    .

/**
 * C4.5
 * Node:        node to build
 * Examples:    training examples
 * Attributes:  list of attributes to be tested
 */
 c45(Node, Examples, Attributes) :- 
    Node = node(NodeName, _, _, _),
    log_d('c45', ['Executing C4.5 for node ', NodeName, '.']),
    % STEP 1: check if every example is in the same class
    % find all the different target values in the current subset
    target_class(TargetAttr),
    findall(Value,  (member(ID, Examples), example(_, ID, TargetAttr, Value)), TargetValues),
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

    % RECURSION STEP: split the examples according to the best attribute
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
                log_d('c45', ['There are no more examples to analyze for attribute ', BestAttribute, ' in range ', Range, '.']),
                list_most_common(TargetValues, MostCommonValue, _),
                assertz(node_label(ChildNode, MostCommonValue))
                ;
                log_d('c45', ['Recursively calling C4.5 on ', ChildNodeName, '.']),
                c45(ChildNode, NewExamples, RemainingAttributes)
            ),
        % loop until all ranges are analyzed and nodes are created
        fail
    )
    .

% always succeed
c45(_,_,_) :- true.

/**
 * TODO: write doc
 */
% funfunfun
learn :-
    log_e('learner', 'YOU DIDN''T SAY THE MAGIC WORD!').

/**
 * TODO: write doc
 */
print_le_tree :-
    RootNode = node('root', root, root, root),
    print_le_branch(RootNode, 0).

/**
 * TODO: write doc
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

% ------------------ %
%   RULE GENERATOR   %
% ------------------ %

/**
 * For each node_label with a positive outcome, generate the corresponding rule
 * by going up from the leaf to the tree root node.
 */
gen_all_the_rulez :- 
    retractall(is_positive(_)),
    log_d('gen_all_rules', 'Starting to generate rules...'),
    positive_target(PositiveID),                % get the positive ID
    node_label(Node, PositiveID),               % for every positive node
    gen_rule(Node),                             % generate corresponding rule
    fail                                        % loop
    ;
    log_i('gen_all_rules', 'Rules generation complete.'),
    true                                        % always succeed
    .

/**
 * Generate the rule for the corresponding input Node.
 *
 * @param Node      The leaf Node that holds the rule information.
 */
gen_rule(Node) :-
    Node = node(Name, _, _, _),
    log_v('gen_rule', ['Generating rule for node ', Name, '...']),
    get_rule_list(Node, [], Conditions),
    assertz(is_positive(ID) :- (check_condition_list(ID, Conditions))),
    log_v('gen_rule', ['Rule for node ', Name, ' generated.'])
    .

/**
 * Builds a list of condition(Attribute, Range).
 * 
 * @param Node      The Node to build the condition list for.
 * @param PrevList  The temporary list for recursion.
 * @param List      The list of conditions to return.
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
 * Check if a given fact with an ID matches all the conditions in the input list.
 * 
 * @param ID        The ID of the fact/3.
 * @param List      The List of condition/2.
 */
check_condition_list(ID, List) :-
    fact(ID, 'ID', ID), !,
    forall(
        member(Condition, List),
        (
            % get Attribute and Range to test
            Condition = condition(Attribute, Range),
            % test the Attribute and the Range
            fact(ID, Attribute, Value),
            log_v('check', ['Testing ID ', ID, ' on Attribute ', Attribute, ' with value ', Value, ' on ', Range, '.']),
            is_in_range(Value, Range)
        )
    )
    .

/**
 * Generate a set of Prolog conditions from a list.
 * 
 * @param List      The list of conditions.
 * @param Set       The set of Prolog conditions.
 */
get_conditions_from_list([Condition], Condition).
get_conditions_from_list([Head | Tail],(Head, OtherConditions)) :-
    get_conditions_from_list(Tail, OtherConditions).
