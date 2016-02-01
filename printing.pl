:- module(printing, [pretty_print/1, pretty_print/2]).

%print_events(+Events)
print_events(Events) :-
    member(event(EID,_, _, Start), Events),
    duration(EID, Duration),
    End is Start+Duration,
    has_exam(CID,EID),
    teaches(LID,CID),
    exam(EID, Exam_name),
    lecturer(LID, Lecturer_name),
    format('~d-~d ~w (~w)~n', [Start, End, Exam_name, Lecturer_name]).

%print_room(+Events)
print_room(Events) :-
    setof(event(EID,_,Day,Start),Start^member(event(EID,RID,Day,Start), Events), Filtered_events), % Gets for each room the exams held in that room
    room(RID, Room_name),
    format('~w:~n', [Room_name]),
    sort(4, @=<, Filtered_events, Sorted_events),
    findall(_,print_events(Sorted_events),_), % Backtrack over every result of print_events
    nl.

%print_day(+Events)
print_day(Events) :-
    setof(event(EID,B,_,Start),member(event(EID,B,Day,Start), Events),Filtered_events), % Gets for each day the exams held on that day
    format('*** Day ~d ***~n~n', [Day]),
    findall(_,print_room(Filtered_events),_). % Backtrack over every result of print_room using the exams held on a certain day

%pretty_print(+S)
pretty_print(schedule(Events)) :-
    findall(_,print_day(Events),_). % Backtrack over every result of print_day

% student_has_exam(+SID,+Events,-Event)
student_has_exam(SID, Events, event(EID, RID, Day, Start)) :-
    member(event(EID, RID, Day, Start), Events),
    has_exam(CID, EID),
    follows(SID, CID).

% pretty_print(+SID, +S)
pretty_print(SID, schedule(Events)) :-
    findall(X,student_has_exam(SID, Events, X), Filtered_events), % only keep exams that the student has
    pretty_print(schedule(Filtered_events)).