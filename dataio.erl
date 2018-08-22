-module(dataio).
-export([data_collector/1]).


%Collections: dictionary of databases received
data_collector(Collections) ->
    receive
        {add, Label, Value} ->
            CurData = maps:get(Label, Collections, []),
            UpCollections = maps:put(Label, [Value | CurData], Collections),
            data_collector(UpCollections);
        {add, Label, Pid, Value} ->
            Dataset = maps:get(Label, Collections, #{}),
            CurData = maps:get(Pid, Dataset, []),
            NewDataset = maps:put(Pid, [Value | CurData], Dataset), % Note that data is stored in reverse order
            UpCollections = maps:put(Label, NewDataset, Collections),
            data_collector(UpCollections);
        {csv, Title} ->
            Labels = maps:keys(Collections),
            %% Filename = "data/" ++ to_str(Title) ++ aux:get_date_string() ++ ".csv",
            Filename = "data/" ++ Title ++ ".csv",
            io:format("#####Writing CSV at ~s ~n~n", [Filename]),
            %% make CSV title
            Header = string:join(lists:map(fun to_str/1, Labels), ","),
            AllValues = lists:map(fun(L) ->
                                          lists:map(fun to_str/1,
                                                    lists:reverse(maps:get(L, Collections)))
                                  end,
                                  Labels),
            CSVs = lists:map(fun(Vs) -> string:join(Vs, ",") end, aux:zipn(AllValues)),
            file:write_file(Filename, io_lib:format("~s", [string:join([Header|CSVs], "\n")])),
            data_collector(#{}); %erase the data after writing on file
        terminate ->
            io:format("TERMINATING DATA COLLECTOR~n"),
            aux:check_unread_msgs(),
            exit(normal);
        _Other ->
            erlang:error(io:format("Data Collector - unrecognized message: ~w~n", [_Other]))
    after 10000 ->
              io:format("Terminating Data Collector due to lack of activity~n"),
              self() ! terminate
    end.

to_str(O) ->
    io_lib:format("~w", [O]).

