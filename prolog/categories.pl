/**
 * <module> categories
 *
 * Module to categorize fetched examples' attributes into categories.
 *
 * @author Francesco Pontillo
 * @license Apache License, Version 2.0
*/

/**
 * data_type(?Attribute, ?Type) is semidet.
 * 
 * Define the type of the attribute data.
 * 
 * @param Attribute         The Attribute name.
 * @param Type              The Type of the Attribute, can be category or number.
 * 
 */
data_type('PatientSex', category).
data_type('PatientRace', category).
data_type('PatientAge', number).
data_type('KTV', number).
data_type('QB', number).
data_type('ProgWeightLoss', number).
data_type('RealWeightLoss', number).
data_type('DeltaWeight', number).
data_type('ProgDuration', number).
data_type('RealDuration', number).
data_type('DeltaDuration', number).
data_type('SAPStart', number).
data_type('SAPEnd', number).
data_type('SAPAverage', number).
data_type('DAPStart', number).
data_type('DAPEnd', number).
data_type('DAPAverage', number).
data_type('BloodVolume', number).
data_type('DeltaBloodFlow', number).
data_type('DeltaUF', number).
data_type('SymptomID', category).

/**
 * target_class(?Attribute) is semidet.
 * 
 * Holds information of the target Attribute to predict for.
 *
 * @param Attribute         The Attribute name holding the target information (always 'SymptomID').
 */
target_class('SymptomID').

/**
 * class(?Attribute, ?RangeList:list) is semidet.
 * 
 * Holds range information for each attribute.
 * Note: only attributes with at least one value will be classified
 * 
 * @param Attribute         The Attribute name.
 * @param RangeList         The (ordered) list of ranges the attribute was split into.
 */
:- dynamic class/2.

/**
 * update_categories is det.
 * 
 * Make ranges for all of the available attributes.
 * For a category: every possible value is both the start and end of the class 
 * For a number: use ranges that span (|max-min|/10) values
 */
update_categories :-
    log_v('categories', 'Updating categories...'),
    (
        % for each data
        data_type(Attribute, Type),
        % make a class and iterate
        make_class(Attribute, Type), fail
    )
    .
% always succeed
update_categories :- 
    log_i('categories', 'Categories updated.').

/**
 * make_class(+Attribute, +Type) is det.
 *
 * Make ranges for a generic Attribute with a given Type.
 * 
 * @param Attribute         The Attribute name.
 * @param Type              The Type of the Attribute.
 */
make_class(Attribute, _) :-
    log_v('categories', ['Making class ', Attribute]),
    % delete old ranges, if any
    retractall(class(Attribute, _)),
    fail
    .
% classify a category attribute
make_class(Attribute, category) :-
    % add all of the ranges from the examples
    example(_, _, Attribute, Value),
    add_to_class(Attribute, Value, Value),
    % and iterate  
    fail
    .
% classify a number attribute
make_class(Attribute, number) :-
    % get the list of values
    findall(Value, example(_, _, Attribute, Value), ValueList),
    % get the maximum and minimum value
    list_max(ValueList, Max), list_min(ValueList, Min),
    % calculate the class dimension
    Difference is abs(Max - Min),
    % get the range span
    get_range_span(Difference, Span),
    % start generating ranges with the given span
    generate_range(Attribute, Min, Max, Span)
    .
% always succeed
make_class(_,_).

/**
 * get_range_span(+Difference, -Span) is det.
 * 
 * Calculate Difference/10 as the range Span.
 * 
 * @param Difference        The difference between a minimum and maximum value.
 * @param Span              The resulting range cardinality.
 */
get_range_span(Difference, Span) :-
    Span is /(Difference,10).

/**
 * generate_range(+Attribute, +Min, +Max, +Span) is det.
 * 
 * Recursively generate ranges for the given Attribute.
 * 
 * @param Attribute         The Attribute to generate ranges for.
 * @param Min               The current minimum value of the range.
 * @param Max               The current maximum value of the range.
 * @param Span              The calculated range cardinality.
 */
%
generate_range(Attribute, Min, Max, Span) :-
    RangeMin = Min, RangeMax is Min + Span,
    % RangeMin must be less or equal than Max
    >=(Max, RangeMin),
    % add the new range to the class
    add_to_class(Attribute, RangeMin, RangeMax),
    % the NewMin is the old RangeMax
    NewMin = RangeMax,
    % generate the next range, until RangeMax passes Max
    generate_range(Attribute, NewMin, Max, Span).

/**
 * add_to_class(+Attribute, +Bottom, +Top) is semidet.
 * 
 * Add the input range to the ranges of the Attribute (class/2).
 * 
 * @param Attribute         The Attribute to add the range to.
 * @param Bottom            The minimum value of the range.
 * @param Top               The maximum value of the range.
 */
add_to_class(Attribute, Bottom, Top) :-
    (class(Attribute, TheRangeList) ->
        % get the current range list or an empty list
        RangeList = TheRangeList;
        RangeList = []
    ),
    % if the current range doesn't already exist
    not(member(range(Bottom, Top), RangeList)),
    % add it
    append(RangeList, [range(Bottom, Top)], NewRangeList),
    % replace the list
    retractall(class(Attribute, RangeList)),
    assertz(class(Attribute, NewRangeList))
    .

/**
 * is_in_range(+Value, ?Range) is nondet.
 * 
 * Check if the given Value is included in the given Range.
 * If Range is a category, Bottom equals Top, so only a check on Bottom = Value is performed.
 * If Range is a number, Value must be higher or equal than Bottom and 
 * 
 * @param Value             A value, can be a number or a category.
 * @param Range             A Range structure, as range(Bottom, Top).
 */ 
is_in_range(Value, Range) :-
    Range = range(Bottom, Top),
    is_in_range(Value, Bottom, Top).
is_in_range(Value, Value, Value) :- !.
is_in_range(Value, Bottom, Top) :-
    Value >= Bottom,
    Value < Top, !.
