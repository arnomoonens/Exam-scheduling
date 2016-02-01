:- module(valid, [delete_first/3, is_valid/1, is_valid/2, no_collision/4, conflicting_event/2, exam_for_students/2, preprocess/0, retract_preprocessed/0]).

% complete_event_info(+EID,+Start,-End, -Course, -Lecturer, -Students)
complete_event_info(EID,Start,End,Course,Lecturer,Students) :-
    duration(EID,Duration),
    plus(Start, Duration, End),
    has_exam(Course, EID),
    teaches(Lecturer, Course),
    findall(X, follows(X, Course), Students).

% no_collision(+Start1, +End1, +Start2, +End2)
no_collision(Start1,_,_,End2) :- Start1 >= End2,!.
no_collision(_,End1,Start2,_) :- Start2 >= End1.

% delete_first(+E,+L1,-L2)
delete_first(E,[E|T],T) :- !.
delete_first(E,[H|T1],[H|T2]) :-
    delete_first(E,T1,T2).

% intersection(+First,+Second,-Intersection)
intersection([H|T], L, [H|U]) :-
    member(H,L),
    intersection(T,L,U).
 intersection([_|T], L, U) :-
    intersection(T,L,U).
 intersection(_,_,[]).

:- dynamic teacher_students_collision/2.
:- dynamic exam_for_students/2.

% assert_possible_collisions_inner(+EID, +Other_EIDs)
assert_possible_collisions_inner(_,[]).
assert_possible_collisions_inner(EID, [EID2 | Tail]) :-
    EID \== EID2,
    not(teacher_students_collision(EID2, EID)),
    has_exam(CID, EID),
    has_exam(CID2, EID2),
    teaches(LID, CID),
    teaches(LID, CID2),
    !,
    assert(teacher_students_collision(EID, EID2)),
    assert_possible_collisions_inner(EID, Tail).

assert_possible_collisions_inner(EID, [EID2 | Tail]) :-
    EID \== EID2,
    not(teacher_students_collision(EID2, EID)),
    exam_for_students(EID, Students),
    exam_for_students(EID2, Students2),
    intersection(Students,Students2,Inter),
    length(Inter, InterLength),
    InterLength > 0,
    !, % avoid backtracking
    assert(teacher_students_collision(EID, EID2)),
    assert_possible_collisions_inner(EID, Tail).

assert_possible_collisions_inner(EID, [_ | Tail]) :- assert_possible_collisions_inner(EID, Tail).

% assert_possible_collisions_outer(+EIDs, +All_EIDs)
assert_possible_collisions_outer([],_).
assert_possible_collisions_outer([EID| Tail], Exams) :-
    assert_possible_collisions_inner(EID, Exams),
    assert_possible_collisions_outer(Tail, Exams).

% assert_exam_students(EIDs)
assert_exam_students([]).
assert_exam_students([EID | Tail]) :-
    has_exam(CID, EID),
    findall(X, follows(X, CID), Students),
    assert(exam_for_students(EID, Students)),
    assert_exam_students(Tail).

:- dynamic preprocess_done/0.

% preprocess()
preprocess :- preprocess_done, !.
preprocess :-
    findall(X,exam(X,_),Exams),
    assert_exam_students(Exams),
    assert_possible_collisions_outer(Exams, Exams),
    assert(preprocess_done).

% retract_preprocessed()
retract_preprocessed :-
    retractall(exam_for_students(_,_)),
    retractall(teacher_students_collision(_,_)),
    retract(preprocess_done).

% conflicting_event(+Event, +Other_events)
%% There is a conflict if they take place at the same time and in the same room or with the same teacher/students
conflicting_event(event(EID,RID,Day,Start), [event(EID2,RID2,Day,Start2) | _]) :-
    duration(EID, Duration),
    End is Start+Duration,
    duration(EID2, Duration2),
    End2 is Start2+Duration2,
    not(no_collision(Start,End,Start2,End2)),
    (RID = RID2; teacher_students_collision(EID, EID2); teacher_students_collision(EID2, EID)),
    !. % avoid backtracking

conflicting_event(X,[_ | Tail]) :- conflicting_event(X, Tail).

% is_valid(?schedule(S))
is_valid(schedule(S)):-
    findall(X,exam(X,_),L),
    is_valid(S,L),
    retract_preprocessed().

% get_availability(+RID, +Day, -Start, -End)
get_availability(RID, Day, Start, End) :-
    availability(RID, Day, Start, End),
    !. % to avoid backtracking in availability

is_valid([],[]).
is_valid([event(EID,RID,Day,Start) | Tail],L) :-
    delete_first(EID,L,L2),
    is_valid(Tail,L2),
    (var(Start); integer(Start)), % If Start is already bound, it should be an integer
    exam_for_students(EID, Students),
    length(Students,NrOfStudents),
    room(RID,_),
    capacity(RID, Cap),
    NrOfStudents =< Cap,
    first_day(First_day),
    last_day(Last_day),
    duration(EID, Duration),
    between(First_day, Last_day, Day),
    get_availability(RID,Day,Av_start,Av_end),
    Max is Av_end - Duration, % Maximum hour that an exam is allowed to start on Day
    between(Av_start,Max,Start),
    preprocess(),
    not(conflicting_event(event(EID,RID,Day,Start),Tail)).