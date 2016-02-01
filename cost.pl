:- module(cost, [cost/2, violates_sc/2]).
:- use_module(valid).

%% :- dynamic sc_lunch_break/3.
%% :- dynamic sc_no_exam_in_period/6.
%% :- dynamic sc_not_in_period/6.
%% :- dynamic sc_same_day/4.
%% :- dynamic sc_b2b/4.
%% :- dynamic sc_correction_time/3.
%% :- dynamic sc_study_time/3.

% b2b_penalty(+PID, +EID1, +Start1, +End1, Event2, -Violated, -Penalty)
b2b_penalty(PID, EID1, Day, Start1, End1, event(EID2, _, Day, Start2), [sc_b2b(PID,EID1,EID2,Penalty)], Penalty) :-
    duration(EID2, Duration),
    End2 is Start2+Duration,
    (Start1 = End2; Start2 = End1),
    sc_b2b(PID, Penalty).
b2b_penalty(_, _, _, _, _, _, [], 0).

% sc_not_in_period(+PID, +EID, +Day, +Start, +End, -Violated, -Penalty)
not_in_period(PID, EID, Day, Start, End, [sc_not_in_period(PID,EID,Day,Start,End,Penalty)], Penalty) :-
    sc_not_in_period(PID, EID, Day, Start2, End2, Penalty),
    not(no_collision(Start, End, Start2, End2)),
    !.

not_in_period(_,_,_,_,_, [], 0).

% lunch_break(+PID, +EID, +Start, +End, -Violated, -Penalty)
lunch_break(PID, EID, Start, End, [sc_lunch_break(PID, EID, Penalty)] , Penalty) :-
    not(no_collision(Start, End, 12, 13)),
    sc_lunch_break(PID, Penalty),
    !.

lunch_break(_, _, _, _, [], 0).

lunch_break(SID, [_ | Tail], Violated, Cost) :- lunch_break(SID, Tail, Violated, Cost).

% combinations_students(+SID, +Event, +Events, +Old_violated, -New_violated, -New_cost)
%% Checks all combinations of events for a student to see if they're being held on the same day
combinations_students(_, _, [], _, [], 0).
combinations_students(SID, event(EID, RID, Day, Start), [event(EID2, _, Day, _) | Rest], Old_violated, New_violated, New_cost) :-
    EID \== EID2,
    not(member(sc_same_day(SID, EID2, EID, _), Old_violated)), % avoid duplicates
    exam_for_students(EID2, Students2),
    member(SID, Students2),
    sc_same_day(SID, Same_day_penalty),
    combinations_students(SID, event(EID, RID, Day, Start), Rest, Old_violated, Violated, Cost),
    New_cost is Cost + Same_day_penalty,
    New_violated = [sc_same_day(SID,EID,EID2,Same_day_penalty) | Violated],
    !.

combinations_students(SID, event(EID, RID, Day, Start), [_ | Rest], Old_violated, Violated, Cost) :- combinations_students(SID, event(EID, RID, Day, Start), Rest, Old_violated, Violated, Cost).

% same_day_students(+SID, +Events, +All_events, -New_violated, -New_cost)
same_day_students(_, [], _, [], 0).
same_day_students(SID, [event(EID, RID, Day, Start) | Rest], Events, New_violated, New_cost) :-
    exam_for_students(EID, Students),
    member(SID, Students),
    same_day_students(SID, Rest, Events, Violated, Cost),
    combinations_students(SID, event(EID, RID, Day, Start), Events, Violated, Combination_violated, Combination_penalties),
    New_cost is Cost + Combination_penalties,
    append(Combination_violated, Violated, New_violated),
    !.

same_day_students(SID, [_ | Tail], Events, Violated, Cost) :- same_day_students(SID, Tail,  Events, Violated, Cost).

% study_time(+SID, +Events, +Last_exam, +Last_exam_day, +Study_days, -New_DTL, -New_cost)
study_time(_,[],_,_,_, 0, 0,[],0).
study_time(SID, [event(EID, RID, Day, Start) | Rest], Last_exam, Last_exam_day,Study_Days, New_DTL, New_cost, New_violated_comb, New_cost_comb) :-
    exam_for_students(EID, Students),
    member(SID, Students),
    Days_available is Study_Days + (Day - Last_exam_day),
    sc_study_time(EID, Days_needed),
    Available_minus_needed is Days_available - Days_needed,
    Days_left is max(0,Available_minus_needed),
    Penalty_days is min(0,Available_minus_needed),
    sc_study_penalty(SID, Study_penalty),
    duration(EID, Duration),
    End is Start+Duration,
    not_in_period(SID, EID, Day, Start, End, Period_violated, Period_cost),
    lunch_break(SID, EID, Start, End, Lunch_violated, Lunch_cost),
    b2b_penalty(SID, EID, Day, Start, End, Last_exam, B2B_violated, B2B_cost),
    study_time(SID, Rest, event(EID, RID, Day, Start), Day, Days_left, DTL, Cost, Old_violated_comb, Old_cost_comb),
    New_cost is Cost + (Study_penalty * abs(Penalty_days)),
    New_DTL is DTL + abs(Penalty_days),
    New_cost_comb is Old_cost_comb + Period_cost + Lunch_cost + B2B_cost,
    append(Old_violated_comb, Period_violated, Violated_with_period),
    append(Violated_with_period, Lunch_violated, Violated_with_lunch),
    append(Violated_with_lunch, B2B_violated, New_violated_comb),
    !.

study_time(SID, [_ | Rest], Event, Last_exam_day,Study_Days, DTL, Cost, Violated_comb, Cost_comb) :- study_time(SID, Rest, Event, Last_exam_day, Study_Days, DTL, Cost, Violated_comb, Cost_comb).

% sc_study_time_if_necessary(+SID, +DTL, +Study_time_cost, -Violated)
%% Only add a soft constraint violation to the list if the study time cost is non-zero
sc_study_time_if_necessary(_, _, 0, []) :- !.
sc_study_time_if_necessary(SID, DTL, Study_time_cost, [sc_study_time(SID, DTL, Study_time_cost)]).

% students_cost(+SIDs, +Events, -New_violated, -New_cost)
students_cost([], _, [], 0).
students_cost([SID | Rest], Events,New_violated, New_cost) :-
    same_day_students(SID, Events, Events, Same_day_violated, Same_day_cost),
    first_day(First_exam_day),
    study_time(SID, Events, nil, First_exam_day, 0, DTL, Study_time_cost, Violated_comb, Cost_comb),
    sc_study_time_if_necessary(SID, DTL, Study_time_cost, Study_time_violated),
    students_cost(Rest, Events, Violated, Cost),
    New_cost is Cost + Cost_comb + Study_time_cost + Same_day_cost,
    append(Violated, Violated_comb, Violated_with_comb),
    append(Violated_with_comb, Same_day_violated, Violated_with_same_day),
    append(Violated_with_same_day, Study_time_violated, New_violated),
    !.

% exam_in_period_lecturer(+LID, +EID, +Day, +Start, +End, -Violated, -Penalty)
exam_in_period_lecturer(LID, EID, Day, Start, End, [sc_no_exam_in_period(LID,EID,Day,Start,End,Penalty)], Penalty) :-
    sc_no_exam_in_period(LID, Day, Start2, End2, Penalty),
    not(no_collision(Start, End, Start2, End2)),
    !.
exam_in_period_lecturer(_, _, _, _, _, [], 0).

% correction_time(+LID, +Events, +Last_exam, +Last_exam_day, +Correction_days, -DTL, -Cost, -Violated_combinations, -Cost_combinations)
correction_time(_,[],_,_,_, 0, 0, [], 0) :- !.
correction_time(LID, [event(EID, RID, Day, Start) | Rest], Last_exam, Last_exam_day,Correction_days, New_DTL, New_cost, New_violated_comb, New_cost_comb) :-
    has_exam(CID,EID),
    teaches(LID, CID),
    Days_available is Correction_days + (Last_exam_day - Day), %Last_exam_day and Day in opposite order because we first handle exams that are held later
    sc_correction_time(EID, Days_needed),
    Available_minus_needed is Days_available - Days_needed,
    Days_left is max(0,Available_minus_needed),
    Penalty_days is min(0,Available_minus_needed),
    duration(EID, Duration),
    End is Start+Duration,
    not_in_period(LID, EID, Day, Start, End, Period_violated, Period_cost),
    exam_in_period_lecturer(LID, EID, Day, Start, End, Exam_in_period_violated, Exam_in_period_cost),
    lunch_break(LID, EID, Start, End, Lunch_violated, Lunch_cost),
    b2b_penalty(LID, EID, Day, Start, End, Last_exam, B2B_violated, B2B_penalty),
    sc_correction_penalty(LID, Correction_penalty),
    correction_time(LID, Rest, event(EID, RID, Day, Start), Day, Days_left, DTL, Cost, Old_violated_comb, Old_cost_comb),
    New_cost is Cost + (Correction_penalty * abs(Penalty_days)),
    New_DTL is DTL + abs(Penalty_days),
    New_cost_comb is Old_cost_comb + Lunch_cost + Period_cost + B2B_penalty + Exam_in_period_cost,
    append(Old_violated_comb, Exam_in_period_violated, Violated_with_exam_in_period),
    append(Violated_with_exam_in_period, Lunch_violated, Violated_with_lunch),
    append(Violated_with_lunch, Period_violated, Violated_with_period),
    append(Violated_with_period, B2B_violated, New_violated_comb),
    !.

correction_time(LID, [_ | Rest], Event, Last_exam_day,Study_Days, DTL, Cost, Violated_comb, Cost_comb):- correction_time(LID, Rest, Event, Last_exam_day,Study_Days, DTL, Cost, Violated_comb, Cost_comb).

% sc_correction_if_necessary(+LID, +DTL, +Correction_cost, -Violated_constraints)
%% Only add a soft constraint violation to the list if the correction time cost is non-zero
sc_correction_if_necessary(_, _, 0, []) :- !.
sc_correction_if_necessary(LID, DTL, Correction_cost, [sc_correction_time(LID,DTL,Correction_cost)]).

%lecturers_cost(+LIDs, +Events, -Violated, -Cost)
lecturers_cost([], _, [], 0).
lecturers_cost([LID | Rest], Events, New_violated, New_cost) :-
    last_day(Last_exam_day),
    correction_time(LID, Events, nil, Last_exam_day, 0, DTL, Correction_cost, Violated_comb, Cost_comb),
    sc_correction_if_necessary(LID, DTL, Correction_cost, Correction_violated),
    lecturers_cost(Rest, Events, Violated, Cost),
    New_cost is Cost + Cost_comb + Correction_cost,
    append(Violated, Violated_comb, Violated_with_comb),
    append(Violated_with_comb, Correction_violated, New_violated),
    !.

%violated_sc_cost(+schedule(Events), -Violated, -Cost)
violated_sc_cost(schedule(Events), Violated, Cost) :-
    preprocess(),
    findall(X,student(X,_),Students),
    length(Students,Nr_of_students),
    sort(3, @=<, Events, Sorted_events_students),
    students_cost(Students, Sorted_events_students, Students_violated, Students_cost),
    findall(X, lecturer(X,_), Lecturers),
    length(Lecturers, Nr_of_lecturers),
    sort(3, @>=, Events, Sorted_events_lecturers), % Handle last exams first so the correction time calculation is almost the same as study time
    lecturers_cost(Lecturers, Sorted_events_lecturers, Lecturers_violated, Lecturers_cost),
    append(Students_violated, Lecturers_violated, Violated),
    Cost is ((Students_cost / Nr_of_students) + (Lecturers_cost / Nr_of_lecturers))/2.

% cost(+S, -Cost)
cost(S, C) :- violated_sc_cost(S, _, C).

% violates_sc(+S,-SC)
violates_sc(S, SC) :- violated_sc_cost(S, SC, _).