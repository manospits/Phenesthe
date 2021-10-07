
dependency_aware_parallel_execution(List, _) :-
    current_prolog_flag(cpu_count, 1), !,
    maplist(run_goal, List).

dependency_aware_parallel_execution(List, Options) :-
    current_prolog_flag(cpu_count, N), !,
    must_be(positive_integer, N),
    must_be(list(callable), List),
    length(List, JobCount),
    message_queue_create(Done),
    message_queue_create(Queue),
    WorkerCount is min(N, JobCount),
    create_query_workers(WorkerCount, Queue, Done, Workers, Options),
    feed_goals_with_dependencies(WorkerCount, List, [], JobCount, Queue, Done, cleanup(Workers, Queue)),
    join_all(Workers),
    message_queue_destroy(Queue),
    message_queue_destroy(Done).

run_goal(Goal) :-
    call(Goal), !.


feed_goals_with_dependencies(WorkerCount, [], FinishedGoals, TotalJobCount, Queue, Done, Cleanup):-
    !,length(FinishedGoals, FinishedGoalsCount),
    (
        (
         FinishedGoalsCount<TotalJobCount,
         thread_get_message(Done, done(_Id, _Vars, Phenomenon)),
         feed_goals_with_dependencies(WorkerCount, [], [Phenomenon|FinishedGoals], TotalJobCount, Queue, Done, Cleanup)
        )
        ;
        (
         FinishedGoalsCount=TotalJobCount,
         forall(between(1, WorkerCount, _),
            thread_send_message(Queue, done))
        )
    ).
feed_goals_with_dependencies(WorkerCount, AvailableGoals, FinishedGoals, TotalJobCount,  Queue, Done, Cleanup):-
    get_ready_to_start_goals(AvailableGoals, FinishedGoals, ReadyGoals, RemainingGoals),
    submit_query_goals(ReadyGoals, 1, Queue, _VarList),
    thread_get_message(Done, done(_Id, _Vars, Phenomenon)),
    feed_goals_with_dependencies(WorkerCount, RemainingGoals, [Phenomenon|FinishedGoals], TotalJobCount, Queue, Done, Cleanup),!.


query_worker(Queue, Done) :-
    thread_get_message(Queue, Message),
    debug(concurrent, 'Worker: received ~p', [Message]),
    (   Message = goal(Id, Goal, Vars)
    ->  (   Goal = process_phenomenon(Phenomenon),
            Phenomenon=..[PhenomenonName|Args],
            length(Args,N),
            Goal
        ->  thread_send_message(Done, done(Id, Vars, (PhenomenonName,N))),
            query_worker(Queue, Done)
        )
    ;   true
    ).

create_query_workers(N, Queue, Done, [Id|Ids], Options) :-
    N > 0,
    !,
    thread_create(query_worker(Queue, Done), Id,Options),
    N2 is N - 1,
    create_query_workers(N2, Queue, Done, Ids, Options).

create_query_workers(_, _, _, [], _).


submit_query_goals([], _,  _, []).
submit_query_goals([H|T], I, Queue, [Vars|VT]) :-
    term_variables(H, Vars),
    thread_send_message(Queue, goal(I, H, Vars)),
    I2 is I + 1,
    submit_query_goals(T, I2, Queue, VT).


get_ready_to_start_goals([], _, [], []).
get_ready_to_start_goals([Goal|OtherGoals], FinishedGoals, ReadyGoals, RemainingGoals):-
    Goal=process_phenomenon(Phenomenon),
    dependencies(Phenomenon, DependencyList),
    get_ready_to_start_goals(OtherGoals, FinishedGoals, ReadyGoals1, RemainingGoals1),
    (   all_dependencies_in_finished_goals(DependencyList, FinishedGoals) 
    -> (    ReadyGoals = [Goal|ReadyGoals1], RemainingGoals = RemainingGoals1)
    ; (ReadyGoals=ReadyGoals1, RemainingGoals=[Goal|RemainingGoals1])).


all_dependencies_in_finished_goals([],_).
all_dependencies_in_finished_goals([A|A1],B):-
    (   phenomenon_type(A,_,user) 
    ->  (A=..[PhenomenonName|Args],
         length(Args,N),
         member((PhenomenonName,N),B)
        )
    ; true
    ), all_dependencies_in_finished_goals(A1,B).

join_all([]).
join_all([Id|T]) :-
    thread_join(Id, _),
    join_all(T).
