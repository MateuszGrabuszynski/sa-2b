-module(trade_no_cheat).
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
    timer:sleep(300),
    trade_fsm:accept_trade(Pid),
    timer:sleep(300),
    trade_fsm:make_bid(Pid, "pen", 200),
    timer:sleep(200),
    trade_fsm:make_bid(Pid, "pen", 225),
    timer:sleep(300),
    trade_fsm:ready(Pid),
    timer:sleep(200).

b(PidA, PidCliA) ->
    {ok, Pid} = trade_fsm:start_link("Jim"),
    io:format("Spawned Jim: ~p~n", [Pid]),
    timer:sleep(200),
    trade_fsm:trade(Pid, PidA),
    timer:sleep(200),
    trade_fsm:make_offer(Pid, "pen", 300),
    timer:sleep(400),
    trade_fsm:make_bid(Pid, "pen", 275),
    timer:sleep(200),
    trade_fsm:make_bid(Pid, "pen", 250),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(200).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
