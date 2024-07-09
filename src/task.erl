-module(task).
-export([handle_request/3]).

%% Handle incoming request
handle_request(SessId, _Env, ReqData) ->
    %% Parse JSON body from the request
    Response = handle_json(list_to_binary(ReqData)),

    %% Send response
    mod_esi:deliver(SessId, binary_to_list(Response)).

handle_json(ReqData) ->
    %% Decode JSON content
    Response = jsx:decode(ReqData, [{return_maps, false}]),

    %% Sort JSON data in specific order
    SortedResponse = sort_tasks(Response),

    %% Create complete response and encode to JSON format
    CompleteResponse = {<<"tasks">>, SortedResponse},
    jsx:encode([CompleteResponse], [indent]).

sort_tasks(Task) ->
    %% Extract list of tasks
    ListOfTasks = proplists:get_value(<<"tasks">>, Task),

    %% Split list of tasks to 2 list - with and without requires
    {ListOfDependentTasks, ListOfAloneTasks} = lists:partition(fun(TaskList) -> 
        lists:keymember(<<"requires">>, 1, TaskList)
    end, ListOfTasks),

    %% Compare requires
    %% If length of requires is same check the last element order
    %% Otherwise put task with less no of requires as first one
    SortedDependentTasksList = lists:sort(fun(ListOfTuple1, ListOfTuple2) ->
        {_, Requires1} = lists:keyfind(<<"requires">>, 1, ListOfTuple1),
        {_, Requires2} = lists:keyfind(<<"requires">>, 1, ListOfTuple2),

        case length(Requires1) == length(Requires2) of
            true ->
                lists:nthtail(length(ListOfTuple1)-1, ListOfTuple1) < lists:nthtail(length(ListOfTuple2)-1, ListOfTuple2);
            false -> length(Requires1) < length(Requires2)
        end
    end, ListOfDependentTasks),

    %% Create new list from list of task without requires and from list with sorted tasks with requires
    SortedListOfAllTasks = lists:append(ListOfAloneTasks, SortedDependentTasksList),

    %% Print in shell a bash script representation of commands
    print_tasks_commands(SortedListOfAllTasks, bash),
    
    SortedListOfAllTasks.

print_tasks_commands(ListOfTasks, bash) ->
    io:format("~s~n", ["#!/bin/bash"]),
    print_tasks_commands(ListOfTasks).

print_tasks_commands([FirstElement | Rest]) ->
    case lists:keyfind(<<"command">>, 1, FirstElement) of
        false -> io:format("");
        {_, Val} -> io:format("~s~n", [binary:bin_to_list(Val)])
    end,
    print_tasks_commands(Rest);

print_tasks_commands([]) ->
    ok.