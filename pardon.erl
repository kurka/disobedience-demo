-module(pardon).
-export([main/1, agent/4, game/5]).

-define(POSITION_RULERS, last). %values: [beginning, last, random]
-define(WHO_IS_POLICE, random). %values: [random, rulers, people]
%% -define(FORGIVE_METHOD, general). %values: [general, personal, off]

% game constants
-define(SIMULATION_LENGTH, 2000).
-define(NUM_AGENTS, 50).
-define(RULERS_SIZE, ?NUM_AGENTS div 5). %1/5 of agents are rulers
-define(POLICE_SIZE, ?RULERS_SIZE).


-define(PMONIT, 0.9).
-define(SCARCITY_LEVEL, 0.5). %scarcity constraint: maximum proportion of agents that can be satisfied per turn.
-define(PCHEAT_INIT, 0.0).
-define(SATISF_INIT, 1.0).
-define(REVOL_THRESH, 0.7).


agent(PCheat, AllocSatisf, AppropSatisf, Status) ->
    receive
        {allocation, Allocated, Available} ->
            Appropriation = case aux:flip(PCheat) of %decide if comply or not with allocation
                                true -> 1; % not compliant - appropriate what demanded
                                false -> Allocated % compliant - accept allocation
                            end,
            %% RealAllocated = min(Allocated, Available), %if Available is zero, real allocation is 0
            RealApprop = min(Appropriation, Available), %if Available is zero, real appropriation is 0
            master ! RealApprop, % appropriate resources unless there is none left
            % Update satisfaction metric
            Alpha = Beta = 0.1,
            UpAllocSatisf = if Allocated > 0 -> aux:reinforce(AllocSatisf, Alpha, positive);
                               Allocated == 0 -> aux:reinforce(AllocSatisf, Beta, negative)
                            end,
            UpAppropSatisf = if RealApprop > 0 ->  aux:reinforce(AppropSatisf, Alpha, positive);
                                RealApprop == 0 -> aux:reinforce(AppropSatisf, Beta, negative)
                             end,
            agent(PCheat, UpAllocSatisf, UpAppropSatisf, Status);
        {judgment, RulersSatisf, Verdict} ->
            %% Update PCheat based on verdict and fairness perception
            IsFair = case aux:lookup_ets(globalvars, forgiveness_method) of
                         %% if there isnt forgiveness, sanctions should be treated as always being fair
                         off -> true;
                         oppression -> false; %FIXME: dirty fix!
                         _On ->
                             case aux:lookup_ets(globalvars, ideology) of
                                 political -> AllocSatisf > RulersSatisf-0.3;
                                 apolitical -> AppropSatisf > RulersSatisf+0.05
                             end
                     end,
            %% case {aux:lookup_ets(globalvars, forgiveness_method), Verdict} of
            %%     {oppression, replenish} -> io:format("~w", [PCheat]);
            %%     _ -> pass
            %% end,
            case Status of
                _ -> pass
            end,
            UpPCheat =
                case Verdict of
                    sanction ->
                        case IsFair of % judge if the sanction was given rightly and adapt behaviour
                            true -> aux:reinforce(PCheat, 0.05, negative);
                            false -> aux:reinforce(PCheat, 0.05, positive)
                        end;
                    forgive -> PCheat; %keep behaviour if forgiven
                    %% forgive -> aux:reinforce(PCheat, 0.005, negative); %Forgiveness serve as a warning to agents
                    %% forgive -> aux:reinforce(PCheat, 0.005, positive); %Forgiveness serve as incentive to agents
                    notcaught -> aux:reinforce(PCheat, 0.001, positive); %increase PCheat if no one is looking
                    compliant -> aux:reinforce(PCheat, 0.001, negative); %if compliant, decrease PCheat
                    replenish -> aux:reinforce(PCheat, 0.001, negative) %if compliant, decrease PCheat
                end,

            %% Update satisfaction in case of Sanction
            UpAppropSatisf = case Verdict of
                                 sanction -> 0.9*(AppropSatisf-0.1);
                                 replenish -> (AppropSatisf+0.1);
                                 _ -> AppropSatisf
                             end,
            agent(UpPCheat, AllocSatisf, UpAppropSatisf, Status);
        allocsatisfaction -> %return satisfaction when asked
            master ! {allocsatisfaction, AllocSatisf},
            agent(PCheat, AllocSatisf, AppropSatisf, Status);
        appropsatisfaction -> %return satisfaction when asked
            master ! {appropsatisfaction, AppropSatisf},
            agent(PCheat, AllocSatisf, AppropSatisf, Status);
        pcheat -> %return satisfaction when asked
            master ! {pcheat, PCheat},
            agent(PCheat, AllocSatisf, AppropSatisf, Status);
        {resetstats, NewPCheatInit} -> %reset agent parameters, after revolution
            agent(NewPCheatInit, ?SATISF_INIT, ?SATISF_INIT, Status);
        terminate ->
            aux:check_unread_msgs(),
            exit(normal);
        _Other ->
            erlang:error(io:format("Agent - unrecognized message: ~w~n", [_Other]))
    end.


game(AgentsQueue, Rulers, _Police, AllocUnfairness, 0) ->
    People = AgentsQueue--Rulers,
    PCheatFinal = aux:average(aux:ask(People, pcheat)), %get people's pcheat as metric for pcheat
    aux:broadcast(AgentsQueue, terminate),
    aux:check_unread_msgs(),
    case aux:lookup_ets(globalvars, report_type) of
        time -> main ! endgame;
        {trajectory, _RunId} -> main ! endgame;
        phase -> main ! {endgame, PCheatFinal, AllocUnfairness}
    end,
    exit(normal);

game(AgentsQueue, Rulers, Police, AllocUnfairness, TurnNumber) ->
    %% ALLOCATION
    TurnResources = floor(?SCARCITY_LEVEL*length(AgentsQueue)),
    Allocations = allocate(AgentsQueue, Rulers, TurnResources, AllocUnfairness),
    %% APPROPRIATION
    Appropriations = appropriation(Allocations, TurnResources),
    %% MONITORING/SANCTIONING/PARDONING
    police(AgentsQueue, Police, Appropriations, Allocations, Rulers),
    PeopleNC = turn_stats(AgentsQueue, Allocations, Appropriations, Rulers, AllocUnfairness, TurnNumber),
    UpAllocUnfairness = update_alloc_strategy(AllocUnfairness, PeopleNC),
    {UpAgentsQueue, UpRulers, UpPolice, UpUpAllocUnfairness} = consider_revolution(AgentsQueue, Rulers, Police, PeopleNC, UpAllocUnfairness),
    game(UpAgentsQueue, UpRulers, UpPolice, UpUpAllocUnfairness, TurnNumber-1).


allocate(Queue, Rulers, NPool, AllocUnfairness) ->
    People = Queue -- Rulers,  %%TODO: move this computation to outside the loop for performance?
    NAgents = length(Queue),
    NRulers = length(Rulers),
    NPeople = length(People),
    %% number of allocations for each group, under fair regime
    Prob0 = NPool/NAgents, %unbiased chance of receiving a resource (equal to Scarc.level)
    NAllocRFair = Prob0*NRulers,
    NAllocPFair = Prob0*NPeople,
    %% change the numbers above, depending on AllocUnfairness index
    NPriviledged = (NRulers-NAllocRFair)*AllocUnfairness, %number of extra rulers that will receive resources
    NAllocR = floor(NAllocRFair + NPriviledged),
    NAllocP = floor(NAllocPFair - NPriviledged),

    Receivers = aux:chooseNRandom(NAllocR, Rulers) ++ aux:chooseNRandom(NAllocP, People),
    ShuffledQueue = case aux:lookup_ets(globalvars, rulersposition) of
                        random -> aux:shuffle(Queue);
                        beginning -> aux:shuffle(Rulers) ++ aux:shuffle(People);
                        last -> aux:shuffle(People) ++ aux:shuffle(Rulers)
                    end,

    lists:map(fun(Pid) ->
                      case lists:member(Pid, Receivers) of
                          true -> {Pid, 1}; %receive allocation
                          false -> {Pid, 0} %dont receive allocation
                      end
              end,
              ShuffledQueue).


appropriation(Allocations, Pool) ->
    %% for each agent, send allocation and get appropriation
    {_Left, RevApprop} = lists:foldl(
                           fun({Pid, Alloc}, {ResAvail, Appropriations}) ->
                                   Pid ! {allocation, Alloc, ResAvail},
                                   receive R_ -> {max(ResAvail - R_, 0), [{Pid, R_}|Appropriations]} end
                           end,
                           {Pool, []},
                           Allocations),
    lists:reverse(RevApprop).

police(Agents, Police, Appropriations, Allocations, Rulers) ->
    PObs = ?PMONIT,
    SatisfMetric = case aux:lookup_ets(globalvars, ideology) of
                       political -> allocsatisfaction;
                       apolitical -> appropsatisfaction
                   end,
    AllSatisfactions = aux:ask(Agents, allocsatisfaction),
    RulersSatisfactions = aux:average(aux:ask(Rulers, SatisfMetric)),
    PForg = case aux:lookup_ets(globalvars, forgiveness_method) of
                general -> aux:gini(AllSatisfactions)*2; %Probability of forgiveness is a function of Gini index
                personal -> 1.0-aux:average(aux:ask(Police, SatisfMetric)); %Probability is equal to avg police satisfaction
                off -> 0.0; %don't use forgiveness in this game (base game)
                oppression -> 0.0 %don't forgive if under oppression (variation of base game)
            end,
    %% OrdApprop = lists:keysort(1, Appropriations),
    %% OrdAlloc = lists:keysort(1, Allocations),
    lists:foldl(
      fun({{Pid, AP}, {Pid, AL}}, Replenished) ->
              case AP>AL of %check if was compliant
                  true -> %non-compliant
                      case aux:flip(PObs) of %try to monitor
                          true -> % monitored and catched non-compliance
                              case aux:flip(PForg) of
                                  false ->
                                      Pid ! {judgment, RulersSatisfactions, sanction},
                                      Replenished+1; %restore what was taken
                                  true ->
                                      Pid ! {judgment, RulersSatisfactions, forgive},
                                      Replenished
                              end;
                          false ->
                              Pid ! {judgment, RulersSatisfactions, notcaught}, %non-compliance wasn't caught
                              Replenished
                      end;
                  false -> %compliant
                      case (Replenished>0) and (AL==1) and (AP==0) of
                          true ->
                              Pid ! {judgment, RulersSatisfactions, replenish},
                              Replenished-1; %use replenish
                          false ->
                              Pid ! {judgment, RulersSatisfactions, compliant}, %compliant behaviour
                              Replenished
                      end
              end
      end,
      0,
      lists:zip(Appropriations, Allocations)).

turn_stats(Agents, Allocations, Appropriations, Rulers, AllocUnfairness, TurnNumber) ->
    People = Agents--Rulers,
    case aux:lookup_ets(globalvars, report_type) of
        time ->
            PCheatsPeople = aux:ask(People, pcheat),
            %% PCheatsRulers = aux:ask(Rulers, pcheat),
            %% PCheats = PCheatsPeople ++ PCheatsRulers,

            SatisfPeople = aux:ask(People, appropsatisfaction),
            SatisfRulers = aux:ask(Rulers, appropsatisfaction),
            Satisfactions = SatisfPeople ++ SatisfRulers,
            %% mean compliance probability
            dataio ! {add, people_noncompliance, aux:average(PCheatsPeople)},
            %% dataio ! {add, rulers_noncompliance, aux:average(PCheatsRulers)},
            %% dataio ! {add, mean_noncompliance, aux:average(PCheats)},
            %% Gini coefficient
            dataio ! {add, inequality, aux:gini(Satisfactions)/0.5},
            %% Satisfied agents
            %% dataio ! {add, mean_satisfactions, aux:average(Satisfactions)},
            dataio ! {add, satisf_clique, aux:average(SatisfRulers)},
            dataio ! {add, satisf_people, aux:average(SatisfPeople)},
            %% Disobedience count
            %% Just consider agents which allocation is equal to 0 (so they have the option to disobey)
            PossCheaters = lists:filtermap(fun({Pid, AL}) ->
                                                   case AL of
                                                       0 -> {true, Pid}; %return Pid when alloc==0
                                                       _ -> false
                                                   end
                                           end,
                                           lists:keysort(1, Allocations)),
            PeoplePredicate = fun({Pid, _}) ->
                                      (lists:member(Pid, PossCheaters)) and (lists:member(Pid, People))
                              end,
            %% RulersPredicate = fun({Pid, _}) ->
            %%                           (lists:member(Pid, PossCheaters))
            %%                               and (lists:member(Pid, Rulers))
            %%                   end,
            AllocCheaters = lists:filter(PeoplePredicate, lists:keysort(1, Allocations)),
            AppropCheaters = lists:filter(PeoplePredicate, lists:keysort(1, Appropriations)),

            Disobedient = aux:average(lists:zipwith(fun({Pid, AL}, {Pid, AP}) ->
                                                            case ((AP>AL) and (AL==0)) of
                                                                true -> 1;
                                                                false -> 0
                                                            end
                                                    end,
                                                    AllocCheaters,
                                                    AppropCheaters)
                                      %% lists:keysort(1, Allocations),
                                      %% lists:keysort(1, Appropriations))
                                     ),
            dataio ! {add, governability, 1.0-Disobedient},
            dataio ! {add, alloc_unfairness, AllocUnfairness},
            aux:average(PCheatsPeople);
        {trajectory, RunId} ->
            CheatPeople = aux:average(aux:ask(People, pcheat)),
            case TurnNumber rem 1 == 0 of
                true ->
                    dataio ! {add, id, RunId},
                    dataio ! {add, alloc_unfairness, AllocUnfairness},
                    dataio ! {add, pcheat, CheatPeople};
                false ->
                    pass % wait more to send data
            end,
            CheatPeople;
        phase ->
            PCheatsPeople = aux:ask(People, pcheat),
            aux:average(PCheatsPeople)
    end.


consider_revolution(CurAgentsQueue, CurRulers, CurPolice,
                    Metric, AllocUnfairness) ->
    %% based on system parameters, decide if a revolution should be done to fix the system

    %% compare the Metric to a threshold
    RevolThreshold = case aux:lookup_ets(globalvars, reformation) of
                         on -> ?REVOL_THRESH;
                         off -> 1.0
                     end,

    %% io:format("R: ~w ~w ~w", [Metric, RevolThreshold, Metric>RevolThreshold]),
    case Metric > RevolThreshold of
        true ->  %% revolution! change rulers and police
            revolution(CurAgentsQueue, CurRulers);
        false ->  %% stability - keep the rulers and police as it is
            {CurAgentsQueue, CurRulers, CurPolice, AllocUnfairness}
    end.



revolution(AllAgents, CurRulers) ->
    %% it's revolution, baby!
    People = AllAgents -- CurRulers,
    %% choose new rulers exclusively among people (TODO: should we consider repeating rulers too?)
    NewRulers = aux:chooseNRandom(?RULERS_SIZE, People),
    NewPeople = AllAgents -- NewRulers,
    NewAgentsQueue = case aux:lookup_ets(globalvars, rulersposition) of
                         random -> aux:shuffle(NewRulers++NewPeople);
                         beginning -> aux:shuffle(NewRulers) ++ aux:shuffle(NewPeople);
                         last -> aux:shuffle(NewPeople) ++ aux:shuffle(NewRulers)
                     end,
    NewPolice = case aux:lookup_ets(globalvars, whoispolice) of
                    random -> aux:chooseNRandom(?POLICE_SIZE, NewAgentsQueue);
                    rulers -> aux:chooseNRandom(?POLICE_SIZE, NewRulers);
                    people -> aux:chooseNRandom(?POLICE_SIZE, NewPeople)
                end,
    %% new regime starts with random PCheatInit and AllocUnfairness
    NewGenerationPCheatInit = rand:uniform()*?REVOL_THRESH,
    NewAllocUnfairness = rand:uniform(),
    aux:broadcast(NewAgentsQueue, {resetstats, NewGenerationPCheatInit}),
    % update trajectory Id case tracking trajectories
    case aux:lookup_ets(globalvars, report_type) of
        {trajectory, RunId} ->
            ets:insert(globalvars, {report_type, {trajectory, RunId+1}});
        _ -> pass
    end,
    {NewAgentsQueue, NewRulers, NewPolice, NewAllocUnfairness}.


update_alloc_strategy(AllocUnfairness, Metric) ->
    case aux:lookup_ets(globalvars, updatealloc) of
        off ->
            AllocUnfairness; %don't change U
        fixed -> % always increase unfairness
            aux:reinforce(AllocUnfairness, 0.01, positive);
        adaptive ->
            case Metric > ?REVOL_THRESH - 0.2 of
                true ->
                    aux:reinforce(AllocUnfairness, 0.1, negative);
                false ->
                    aux:reinforce(AllocUnfairness, 0.1, positive)
            end
    end.


sample_run(AllocUnfairness, RulersPosition, WhoIsPolice,
           ForgivenessMethod, Reformation, UpdateAlloc,
           PCheatInit, SatisfInit, PMonit,
           NAgents, RulersSize, PoliceSize, SimLength,
           ReportType, Ideology) ->

    ets:new(globalvars, [set, public, named_table]),
    ets:insert(globalvars, {forgiveness_method, ForgivenessMethod}),
    ets:insert(globalvars, {reformation, Reformation}),
    ets:insert(globalvars, {pmonit, PMonit}),
    ets:insert(globalvars, {pcheatinit, PCheatInit}),
    ets:insert(globalvars, {whoispolice, WhoIsPolice}),
    ets:insert(globalvars, {rulersposition, RulersPosition}),
    ets:insert(globalvars, {updatealloc, UpdateAlloc}), % define if alloc policy is updated. Values: [on, off]
    ets:insert(globalvars, {report_type, ReportType}),
    ets:insert(globalvars, {ideology, Ideology}),

    %% register(dataio, spawn(dataio, data_collector, [#{}])),
    %% register(main, self()),

    AgentsNumber = lists:seq(1, NAgents),
    % Define who will decide the game's allocation
    RulersId = case RulersPosition of
                 beginning -> lists:sublist(AgentsNumber, RulersSize);
                 last -> lists:nthtail(NAgents-RulersSize, AgentsNumber);
                 random -> aux:chooseNRandom(RulersSize, AgentsNumber)
             end,
    %% AgentsQueue = aux:repeat_exec(NAgents,
    %%                               fun(_) -> spawn(pardon, agent, [PCheatInit, SatisfInit, SatisfInit]) end),
    AgentsQueue = lists:map(
                    fun(Id) ->
                            case lists:member(Id, RulersId) of
                                true ->
                                    spawn(pardon, agent, [0.0, SatisfInit, SatisfInit, ruler]);
                                false ->
                                    spawn(pardon, agent, [PCheatInit, SatisfInit, SatisfInit, people])
                            end
                    end,
                            AgentsNumber
                   ),
    Rulers = lists:filtermap(fun({Pid, Id}) ->
                                     case lists:member(Id, RulersId) of
                                         true -> {true, Pid};
                                         false -> false
                                     end
                             end,
                             lists:zip(AgentsQueue, AgentsNumber)),
    %% People = lists:filter(fun(Pid) -> not lists:member(Pid, Rulers) end,
    %%                       AgentsQueue),
    %% hd(People) ! {debug, "F"},
    %% lists:last(People) ! {debug, "L"},
    % Define who will police (monitor+sanction) the game
    Police = case WhoIsPolice of
                 random -> aux:chooseNRandom(PoliceSize, AgentsQueue); %choose random agents (rulers or not)
                 rulers -> aux:chooseNRandom(PoliceSize, Rulers); %the rulers are also the police
                 people -> aux:chooseNRandom(PoliceSize, AgentsQueue--Rulers) %police is composed strictly by non rulers
             end,
    register(master, spawn(pardon, game, [AgentsQueue, Rulers, Police, AllocUnfairness, SimLength])),

    receive
        endgame -> ok;
        {endgame, FinalPCheat, FinalU} ->
            dataio ! {add, uinit, AllocUnfairness},
            dataio ! {add, ufinal, FinalU},
            dataio ! {add, pinit, PCheatInit},
            dataio ! {add, pfinal, FinalPCheat}
    after 1000*20 ->
            io:format("Timeout function~n"),
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, AgentsQueue),
            exit(whereis(master), kill),
            exit(whereis(dataio), kill),
            erlang:error(io:format("Timeout! iteration took to long to finish!"))
    end,
    ets:delete(globalvars).


main([AllocUnfairness, PCheatInit,
      ForgivenessMethod, Reformation,
      UpdateAlloc, Ideology]) ->

    register(main, self()),
    register(dataio, spawn(dataio, data_collector, [#{}])),

    sample_run(list_to_float(atom_to_list(AllocUnfairness)), ?POSITION_RULERS,
               ?WHO_IS_POLICE, ForgivenessMethod, Reformation, UpdateAlloc,
               list_to_float(atom_to_list(PCheatInit)), ?SATISF_INIT, ?PMONIT,
               ?NUM_AGENTS, ?RULERS_SIZE, ?POLICE_SIZE, ?SIMULATION_LENGTH,
               time , Ideology),

    dataio ! {csv, "livedata"},
    dataio ! terminate,
    %% exit(normal);
    pass;


main(WrongParams) ->
    io:format("Wrong params! ~w", [WrongParams]).

%% Presets:
%%     %% U  PCheat, Forg, Reform, UpAlloc
%%     [{0.0, 0.0, off, off, off}, % base fair
%%      {0.0, 0.5, off, off, off}, % base unjustified
%%      {1.0, 0.0, off, off, off}, % base exploitation
%%      {1.0, 1.0, oppression, off, off}, % base oppression
%%      {1.0, 0.1, general, off, off}, % ext1 revolution
%%      %% {1.0, 0.2, general, on, fixed}, % ext2 cycle corrupted
%%      {1.0, 0.2, general, on, adaptive}, % tories
%%      {1.0, 0.2, general, on, off, apolitical} % apolitical
%%     ],
