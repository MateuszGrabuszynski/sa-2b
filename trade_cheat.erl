-module(trade_cheat).
-compile(export_all).

%% test a little bit of everything and also deadlocks on ready state
%% -- leftover messages possible on race conditions on ready state
main_ab() ->
    S = self(),
    PidCliA = spawn(fun() -> a(S) end),
    receive PidA -> PidA end,
    spawn(fun() -> b(PidA, PidCliA) end).

a(Parent) ->
    Pid = spawn(fun() -> c() end),
    Parent ! Pid,
    io:format("Spawned Carl: ~p~n", [Pid]),
    %sys:trace(Pid,true),
    timer:sleep(300),
    io:format("Accepting trade for pen ($500) from Jim~n"),
    timer:sleep(200),
    io:format("Jim is ready~n"),
    timer:sleep(200),
    io:format("Carl bids $300 for pen~n"),
    timer:sleep(200),
    io:format("Jim wants $450 but Carl bids $325~n"),
    timer:sleep(200),
    io:format("Jim wants $400 but Carl bids $350~n"),
    timer:sleep(200),
    io:format("Jim agreed for $350. Carl sends the money to Jim.~n"),
    timer:sleep(200),
    io:format("Jim gave Carl the pen. Transaction completed.~n").

b(PidA, PidCliA) ->
    Pid = spawn(fun() -> c() end),
    io:format("Spawned Jim: ~p~n", [Pid]),
    timer:sleep(200),
    io:format("Jim is willing to trade pen for $500.~n"),
    timer:sleep(300),
    io:format("Carl is ready. Telling Jim I am ready.~n"),
    timer:sleep(300),
    io:format("Carl bided $300 but nope, I want $450!~n"),
    timer:sleep(200),
    io:format("Carl bided $325 but nope, I want $400!~n"),
    timer:sleep(200),
    io:format("Carl bided $350. OK LET'S DO THIS! state->ready~n"),
    timer:sleep(200),
    io:format("Carl sent $350. Jim gives Carl the pen.~n").

c() ->
  timer:sleep(1).

%%% Utils
sync1(Pid) ->
    Pid ! self(),
    receive ack -> ok end.

sync2() ->
    receive
        From -> From ! ack
    end.
