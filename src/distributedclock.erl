-module(distributedclock).
-author("miwo").
-export([clock/1, mainEnhanced/0, clockEnhanced/0, ticker/2]).

clock(TimeInterval) ->
  clock(0, false, TimeInterval).
clock(CurrentTime, IsPaused, TimeInterval) ->
  receive

    {set, Value} ->
      clock(Value, IsPaused, TimeInterval);

    {get, Pid} ->
      Pid ! {clock, CurrentTime, IsPaused, TimeInterval},
      clock(CurrentTime, IsPaused, TimeInterval);

    pause ->
      clock(CurrentTime, true, TimeInterval);

    resume ->
      clock(CurrentTime, false, TimeInterval);

    tick ->
      if IsPaused == true -> clock(CurrentTime, IsPaused, TimeInterval);
        IsPaused == false -> clock(CurrentTime + TimeInterval, IsPaused, TimeInterval) end;

    stop -> exit(ok)
  end.

-define(initialTime, 100).

clockEnhanced() ->
  Clock_Process = spawn(?MODULE, clock, [?initialTime]),
  spawn(?MODULE, ticker, [Clock_Process, ?initialTime]),
  Clock_Process.

ticker(ClockPID, TimeInterval) ->
  receive
  after
    TimeInterval -> ClockPID ! tick, ticker(ClockPID, TimeInterval)
  end.

mainEnhanced() ->
  Clock_Process = clockEnhanced(),
  timer:sleep(1000),
  Clock_Process ! {get, self()},
  io:format("~p", [receive MSG -> MSG end]),
  Clock_Process ! pause,
  timer:sleep(1000),
  Clock_Process ! {get, self()},
  io:format("~p", [receive MSG1 -> MSG1 end]),
  Clock_Process ! stop.