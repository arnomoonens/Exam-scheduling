:- module(heuristically, [find_heuristically/1, find_heuristically/2]).
:- use_module(valid).
:- use_module(cost).

% copy_to_list(+Item, +N, -List)
copy_to_list(_,0,[]) :- !.
copy_to_list(Item,Counter,New_list) :-
    New_counter is Counter - 1,
    copy_to_list(Item, New_counter, List),
    New_list = [Item | List].

% find_best(+Schedules_costs, -Best)
find_best([X | Tail], Best) :-
    find_best(Tail, X, Best).

% find_best(+Schedules_costs, +Current_best, -Best)
find_best([], Best, Best).

find_best([(S, New_cost) | Tail], (_, Current_cost), Best) :-
    cost(S, New_cost),
    New_cost < Current_cost,
    find_best(Tail, (S, New_cost), Best),
    !.

find_best([_ | Tail], Current, Best) :-
    find_best(Tail, Current, Best).

% random_element(+List, -Element)
random_element(List, Element) :-
    length(List, Length),
    random(0,Length,Index),
    nth0(Index,List,Element).

% mutate_schedules(+Original_list, -Mutated_list)
mutate_schedules([], []).
mutate_schedules([(schedule(X), Cost) | Rest], New_list) :-
    mutate_schedules(Rest, List),
    random_element(X, event(EID, RID, Day, Start)),
    first_day(First_day),
    last_day(Last_day),
    delete_first(event(EID, RID, Day, Start), X, Filtered),
    findall(D,between(First_day, Last_day,D),Range),
    random_permutation(Range, Permutation), % used in combination with member/2 so backtracking can be used
    room(RID2,_),
    member(Day2, Permutation),
    availability(RID2,Day2,Av_start,Av_end),
    duration(EID, Duration),
    Max is Av_end - Duration,
    findall(H,between(Av_start,Max,H), Hours),
    random_permutation(Hours, Hours_permutation),
    member(Start2, Hours_permutation),
    (Day =\= Day2; Start =\= Start2), % There must be a difference between the old and the new schedule
    not(conflicting_event(event(EID,RID2,Day2,Start2),Filtered)),
    cost(schedule([event(EID, RID2, Day2, Start2) | Filtered]), New_cost),
    New_list = [(schedule([event(EID, RID2, Day2, Start2) | Filtered]), New_cost), (schedule(X), Cost) | List]. % Add the old and new schedule to the new list

filter_n_bests([],_,[]).
filter_n_bests(_, 0, []).
filter_n_bests([X | Rest], Counter, New_list) :-
    New_counter is Counter - 1,
    filter_n_bests(Rest, New_counter, List),
    New_list = [X | List].

% find_heuristically(-S,+T)
find_heuristically(S,T) :-
    get_time(Start_time),
    findall(X,exam(X,_),L), % use is_valid/2 to avoid retracting
    is_valid(Events,L),
    cost(schedule(Events), Cost),
    copy_to_list((schedule(Events), Cost), 3, List),
    localsearch(List, Start_time, S, T),
    retract_preprocessed(),
    !.

% localsearch(+List, +Start_time, -S, +T)
localsearch(List, Start_time, S, T) :-
    get_time(New_time),
    Diff is New_time - Start_time,
    Diff < T,
    !, % Used so you keep searching if there is time left
    mutate_schedules(List, New_list),
    sort(2,@=<,New_list, Sorted),
    filter_n_bests(Sorted, 3, Filtered_list),
    length(Sorted, Length),
    Max is Length - 1,
    random_between(3, Max, Index), %Take a random schedule from the ones that weren't kept using filter_n_bests
    nth0(Index, Sorted, (Extra_S, Extra_Cost)),
    localsearch([(Extra_S, Extra_Cost) | Filtered_list], Start_time, S, T).

localsearch(List, _, S, _) :-
    find_best(List, (S,_)). % Time was: unify S with the schedule with the lowest cost

% find_heuristically(-S)
find_heuristically(S) :-
    find_heuristically(S,110).
