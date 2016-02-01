:- module(optimal, [is_optimal/1, find_optimal/1]).
:- use_module(valid).
:- use_module(cost).

% all_optimals(+Schedule_costs, -Optimals)
all_optimals([Head | Tail], Optimals) :-
    all_optimals(Tail, [Head], Optimals).

%all_optimals(+Schedule_costs, +Current_bests, Optimals)
all_optimals([], Best, Best).

all_optimals([(S, C) | Tail], [(_, Best_cost) | _], Optimals) :-
    C < Best_cost,
    all_optimals(Tail, [(S,C)], Optimals), %new list with new best schedule and cost as only element
    !.

% Cost of S = Best_cost, add schedule and cost to list of the bests
all_optimals([(S, Best_cost) | Tail], [(Best_s, Best_cost) | Rest], Optimals) :-
    all_optimals(Tail, [(S, Best_cost), (Best_s, Best_cost) | Rest], Optimals),
    !.

all_optimals([_ | Tail], Current, Optimals) :-
    all_optimals(Tail, Current, Optimals).

%is_optimal(-Best)
is_optimal(Best) :-
    findall(X,exam(X,_),L),
    findall((schedule(S),C), (is_valid(S,L), cost(schedule(S),C)), Schedule_costs), % use is_valid/2 to avoid the need of preprocessing and retracting it every time
    all_optimals(Schedule_costs, Optimals),
    retract_preprocessed(),
    member((Best,_), Optimals).

%find_optimal(-Best)
find_optimal(Best) :- is_optimal(Best), !.
