-module(trade_check).
-compile(export_all).

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(800),
    trade_fsm:accept_trade(Pid),
    timer:sleep(400),
    io:format("~p~n",[trade_fsm:ready(Pid)]),
    timer:sleep(1000),
    trade_fsm:make_offer(Pid, "horse", 15),
    trade_fsm:make_offer(Pid, "sword", 134059),
    timer:sleep(1000),
    io:format("a synchronizing~n"),
    sync2(),
    trade_fsm:ready(Pid),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(1000).

b(PidA, PidCliA) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(500),
    trade_fsm:trade(Pid, PidA),
    trade_fsm:make_offer(Pid, "boots", 10),
    timer:sleep(200),
    trade_fsm:retract_offer(Pid, "boots"),
    timer:sleep(500),
    trade_fsm:make_offer(Pid, "shotgun", 150),
    timer:sleep(1000),
    io:format("b synchronizing~n"),
    sync1(PidCliA),
%    trade_fsm:make_offer(Pid, "horse"), %% race condition!
    trade_fsm:ready(Pid),
    timer:sleep(200),
    timer:sleep(1000).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
