-module(trade_run).
-compile(export_all).

main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    {ok, Pid} = trade_fsm:start_link("Carl"),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
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
    trade_fsm:make_bid(Pid, "pen", 275),
    timer:sleep(200),
    trade_fsm:make_bid(Pid, "pen", 250),
    timer:sleep(200),
    trade_fsm:ready(Pid),
    timer:sleep(200).
