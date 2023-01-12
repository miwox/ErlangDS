-module(berkleyalgorithm).
-author("miwo").
-export([client_creator/1, client/1, clock/2, main/0, clock_creator/1, ticker/2, daemon/2, receive_skews/1, daemon_creator/0, on_exit/2]).

clock(CurrentTime, IsPaused) ->
  receive
    tick ->
      if IsPaused == true -> clock(CurrentTime, IsPaused);
        IsPaused == false -> clock(CurrentTime + 1, IsPaused) end;

    {set, Value} ->
      clock(Value, IsPaused);

    {get, Pid} ->
      Pid ! {self(), CurrentTime},
      clock(CurrentTime, IsPaused);

    pause ->
      clock(CurrentTime, true);

    resume ->
      clock(CurrentTime, false);

    stop -> exit(ok)
  end.

ticker(ClockPID, TimeInterval) ->
  receive
  after
    TimeInterval -> ClockPID ! tick, ticker(ClockPID, TimeInterval)
  end.

clock_creator(InitialTimeInterval) ->
  Clock_Process = spawn(?MODULE, clock, [0, false]),
  spawn(?MODULE, ticker, [Clock_Process, InitialTimeInterval]),
  Clock_Process.

client(ClockPid) ->
  receive
    {get_skew, Systime, Pid}  -> ClockPid ! {get, self()}, receive
                                                             {_, CurrentTime} -> Pid ! {register, self(), (CurrentTime - Systime)}
                                                           end, client(ClockPid);
    {adjust, Delta}           -> ClockPid ! {get, self()}, receive
                                                             {_, CurrentTime} -> ClockPid ! {set, (CurrentTime + Delta)}
                                                           end, client(ClockPid)
  end.

client_creator(InitialTimeInterval) ->
  Clock_Process = clock_creator(InitialTimeInterval),
  spawn(?MODULE, client, [Clock_Process]).

daemon(Clients, Clock) ->
  receive
    synchronize -> Self = self(), Tag = make_ref(), Clock ! {get, Self}, CurrentTime = receive
                                                                                         {_, ClockTime} -> ClockTime
                                                                                       end,
      io:format("Currenttime of Daemon is ~p \n", [CurrentTime]),
      Tags = lists:map(fun(Client) -> spawn(fun()-> Client ! {get_skew, CurrentTime, Self} end), Tag end, Clients),
      Skews = receive_skews(Tags),
      Clock_Clients = lists:map(fun({Pid, Skew}) -> {Pid, CurrentTime + Skew} end, Skews),
      List_Client_Times = lists:map(fun({_, Skew}) -> Skew end, Clock_Clients),
      AverageTime = (lists:sum(List_Client_Times) + CurrentTime) / (lists:flatlength(List_Client_Times) + 1),
      io:format("Average time is: ~p \n", [AverageTime]),
      Clock_Clients_Adjusted = lists:map(fun({Pid, Time}) -> {Pid, AverageTime - Time} end, Clock_Clients),
      io:format("Clients and have to adjust their clocks with: ~p \n", [Clock_Clients_Adjusted]),
      Daemon_Clock_Delta = AverageTime - CurrentTime,
      io:format("Daemon has to adjust its clock with: ~p \n", [Daemon_Clock_Delta]),
      Clock ! {set, CurrentTime + Daemon_Clock_Delta},
      Clock ! {get, Self}, TimeAfterAdjustment = receive
                                                   {_, ClockTime2} -> ClockTime2
                                                 end,
      io:format("Time of Daemon after adjustment is ~p \n", [TimeAfterAdjustment]),
      lists:map(fun({Pid, Adjustment}) -> spawn(fun()-> Pid ! {adjust, Adjustment} end), Tag end, Clock_Clients_Adjusted),
      daemon(Clients, Clock)
  end.

receive_skews([]) -> [];
receive_skews([_|T]) ->
  Tuple = receive {register, Pid, Skew} -> io:format("Skew: ~p in Client: ~p \n",[Skew, Pid]), {Pid, Skew} end, [Tuple | receive_skews(T)].



daemon_creator() ->
  Clients = [client_creator(2), client_creator(3), client_creator(4), client_creator(2),  client_creator(4),  client_creator(4),  client_creator(11),  client_creator(9)],
  Clock = clock_creator(1),
  spawn(?MODULE, daemon, [Clients, Clock]).

on_exit(Pid, Fun) ->
  spawn(fun() ->
    process_flag(trap_exit, true),
    link(Pid),
    receive
      {'EXIT', Pid, Why} -> Fun(Pid, Why)
    end
        end).


main() ->
  io:format("Start with Daemon....\n"),
  Daemon = daemon_creator(),
  timer:sleep(500),
  on_exit(Daemon, fun(Who, Why) -> io:format(" ~p died with:~p~n",[Who, Why]) end),
  Daemon ! synchronize,
  timer:sleep(100),
  Daemon ! synchronize,
  timer:sleep(300),
  Daemon ! synchronize.