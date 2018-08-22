-module(aux).
-export([repeat_exec/2, broadcast/2, find_and_send_neighbours/3, kneighbours/2,
         average/1, variance/1, std/1, weighted_average/2, update_average/3,
         update_average/4, rolling_average/4, shuffle/1, chooseNRandom/2,
         gini/1, reinforce/3,
         zipn/1, feature_scaling/1, scale_elem/2, scale_min_max/3, invert/1,
         normalize/1, updatenth/3, product/2, product/1, get_date_string/0, div0/2,
         logistic/3, flist2str/1, seq_float/3, flip/1, printif/2, lookup_ets/2,
         check_unread_msgs/0, accumulate_data/2, ask/2, split_per_key/1, keysum/1]).

% Aux functions
repeat_exec(N, Func) ->
    lists:map(Func, lists:seq(0, N-1)).

broadcast(ListAgents, Msg) ->
    lists:foreach(fun(Ag) -> Ag ! Msg end, ListAgents).

find_and_send_neighbours(AllAgents, NNeighbours, Id) ->
    NAgents = length(AllAgents),
    Neis = ordsets:from_list([lists:nth(((I-1+NAgents) rem (NAgents))+1, AllAgents) || I <- lists:seq(Id-NNeighbours, Id+NNeighbours), I /= Id]),
    lists:nth(Id, AllAgents) ! {neighbours, Neis}.


%return tuples of neighbours for each agent of AllAgents list.
kneighbours(AllAgents, NNeighbours) ->
    NAgents = length(AllAgents),
    lists:flatmap(
      fun({IPid,I}) ->
              Nei = ordsets:from_list(
                      [lists:nth(((J-1+NAgents) rem (NAgents))+1, AllAgents)
                       || J <- lists:seq(I-trunc(NNeighbours/2), I+round(NNeighbours/2)), J /= I]),
              lists:zip(lists:duplicate(length(Nei), IPid), Nei)
      end,
      lists:zip(AllAgents, lists:seq(1, NAgents))
     ).

average(X) -> average(X, 0.0, 0.0).

average([H|T], Length, Sum) -> average(T, Length+1, Sum+H);
average([], Length, Sum) -> Sum / Length.

variance(X) -> variance(X, average(X), 0.0, 0.0).

variance([H|T], Mu, Length, VarSum) -> variance(T, Mu, Length+1, VarSum+math:pow(H-Mu, 2));
variance([], _, Length, VarSum) -> VarSum / Length.

std(X) -> math:sqrt(variance(X)).

weighted_average(L, W) ->
    WSum = lists:sum(W),
    Wn = if WSum == 1 -> W; true -> lists:map(fun (Wi) -> Wi/WSum end, W) end,
    lists:foldl(fun({Li, Wi}, WAvg) -> WAvg + Wi*Li end, 0.0, lists:zip(L, Wn)).

update_average(CurAvg, N, NewElem) when N > 0 ->
    ((N-1) * CurAvg + NewElem)/N.

update_average(CurAvg, OldT, NewEvents, DeltaT) ->
    (CurAvg*OldT + NewEvents*DeltaT)/(OldT+DeltaT).

rolling_average(CurAvg, EventsList, NewEvent, Window) when length(EventsList) < Window ->
    {update_average(CurAvg, length(EventsList)+1, NewEvent), [NewEvent|EventsList]};

rolling_average(CurAvg, EventsList, NewEvent, Window) ->
    NewAvg = CurAvg + NewEvent/Window - lists:last(EventsList)/Window,
    NewEventsList = [NewEvent|lists:droplast(EventsList)],
    {NewAvg, NewEventsList}.


shuffle(L) ->
 [X||{_,X} <- lists:sort([{rand:uniform(), N} || N <- L])].

chooseNRandom(N, L) ->
    lists:sublist(shuffle(L), N).

gini(Dist) ->
    Dividend = (2*length(Dist)*lists:sum(Dist)),
    Gini = div0(lists:sum([abs(Xi-Xj) || Xi <- Dist, Xj <- Dist]), Dividend),
    % %%normalize gini
    % div0(length(Dist), (length(Dist)-1))*Gini.
    if Gini>1 -> io:format("~w~nGini: ~p~n", [Dist, Gini]), exit(gini);
       true -> pass
    end,
    Gini.

reinforce(OriginalValue, StepSize, Direction) ->
    case Direction of
        positive ->
            (1-StepSize)*OriginalValue + StepSize;
        negative ->
            (1-StepSize)*OriginalValue
    end.

zipn([[]|_]) -> [];

zipn(LL) ->
  [lists:map(fun hd/1, LL) | zipn(lists:map(fun tl/1, LL))].

feature_scaling(L) ->
    {Min, Max} = find_min_max(L),
    NormFactor = if Max /= Min -> Max-Min; true -> 1.0 end,
    [(El-Min)/NormFactor || El <- L].

scale_elem(E, L) ->
    {Min, Max} = find_min_max(L),
    scale_min_max(E, Min, Max).

scale_min_max(E, Min, Max) ->
    UpMax = max(E, Max),
    UpMin = min(E, Min),
    NormFactor = if UpMax /= UpMin -> UpMax-UpMin; UpMax == UpMin -> 1.0 end,
    (E-UpMin)/NormFactor.


find_min_max([H|T]) ->
    find_min_max(T, H, H).

find_min_max([H|T], Min, Max) ->
    CurMin = if H < Min -> H; H >= Min -> Min end,
    CurMax = if H > Max -> H; H =< Max -> Max end,
    find_min_max(T, CurMin, CurMax);

find_min_max([], Min, Max) -> {Min, Max}.

invert(L) ->
    lists:map(fun(El) -> 1-El end, L).

normalize(L) when L /= [] ->
    SumL = lists:sum(L),
    lists:map(fun(E) -> div0(E,SumL) end, L).

updatenth(NewValue, 1, [_|T]) -> [NewValue|T];
updatenth(NewValue, N, [H|T]) -> [H|updatenth(NewValue, N-1, T)].

%% cartesian product of two lists L1 and L2
product(L1, L2) -> [{E1, E2} || E1 <- L1, E2 <- L2].

%% cartesian product of list of lists
product([H]) -> [[A] || A <- H];
product([H|T]) -> [[A|B] || A <- H, B <- product(T)].

get_date_string() ->
    {{Y,M,D}, {H,Min,_S}} = erlang:localtime(),
    io_lib:format("~B~2..0B~2..0B~2..0B~2..0B", [Y,M,D,H,Min]).

div0(_A, B) when B == 0 -> 0.0;
div0(A, B) -> A/B.

logistic(X, K, X0) ->
    1 - (1 / (1 + math:exp(-K*(X-X0)))).

flist2str(L) ->
    lists:map(fun(E) -> float_to_list(E, [{decimals, 3}]) end, L).

% from https://gist.github.com/andruby/241489
seq_float(Min, Max, Inc, Counter, Acc) when (Counter*Inc + Min) >= Max ->
    lists:reverse([Max|Acc]);
seq_float(Min, Max, Inc, Counter, Acc) ->
    seq_float(Min, Max, Inc, Counter+1, [Inc * Counter + Min|Acc]).
seq_float(Min, Max, Inc) ->
    seq_float(Min, Max, Inc, 0, []).

flip(0.0) -> false;
flip(1.0) -> true;
flip(P) ->
    Draw = rand:uniform(),
    if Draw < P -> true;
       Draw >= P -> false
    end.

printif(Condition, Message) ->
    if Condition ->
            io:format(Message, []);
       not Condition ->
            pass
    end.


lookup_ets(DictName, Key) ->
    [{_, Var}] = ets:lookup(DictName, Key),
    Var.

check_unread_msgs() ->
    {_,UnreadMessages} = process_info(self(), message_queue_len),
    if UnreadMessages > 0 ->
            erlang:error(io:format("Process is finishing with ~p unread messages:~n~p~n",
                                   [UnreadMessages, process_info(self(), messages)]));
       UnreadMessages == 0 ->
            pass
    end.

%% generic function to receive something from all agents
accumulate_data(NMessages, ExpectedMessage) ->
    accumulate_data(NMessages, [], ExpectedMessage).

accumulate_data(0, ReceivedData, _) ->
    ReceivedData;

accumulate_data(NMessages, ReceivedData, ExpectedMessage) ->
    receive
        {ExpectedMessage, Data} ->
            accumulate_data(NMessages-1, [Data|ReceivedData], ExpectedMessage);
        _Other ->
            erlang:error(io:format("Accumulate data - expecting ~w, received ~w",
                                   [ExpectedMessage, _Other])
                        )
    after 1000*10 ->
            erlang:error(io:format("~p: Accumulate data - expecting ~w, timeout!~n", [self(), ExpectedMessage]))
    end.

%send message and wait for response for a list of agents
ask(Pids, Message) ->
    broadcast(Pids, Message),
    accumulate_data(length(Pids), Message).

split_per_key(Data) ->
    lists:foldl(fun({Key, Datai}, SPKM) ->
                        CurData = maps:get(Key, SPKM, []), %get current data for Key (empty array if first one)
                        maps:put(Key, [Datai | CurData], SPKM)
                end,
                maps:new(),
                Data
               ).

keysum(TupleList) ->
    lists:sum(lists:map(fun({_K,V}) -> V end, TupleList)).
