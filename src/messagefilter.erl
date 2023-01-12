-module(messagefilter).
-export([echo/0, start/0, filter_process/2, filter_collector_process/2]).


append([Head|Tail], Element) -> [Head|append(Tail, Element)]; append([], Element) -> [Element].

filter_process(Receiver_id, Nth) ->
  receive
    {filter, Number} -> if
                          Nth rem 2 == 1 -> Receiver_id ! {filter, Number}, filter_process(Receiver_id, Nth + 1);
                          true -> filter_process(Receiver_id, Nth + 1)
                        end;
    {set_sender, Pid} -> filter_process(Pid, Nth);
    UnexpectedMessage -> io:format("Unexpected Message: ~p, but I will  continue \n", [UnexpectedMessage]), filter_process(Receiver_id, Nth + 1)
  end.

filter_collector_process(Receiver_id, List)->

  receive
    {set_sender, Pid} -> filter_collector_process(Pid, List);
    reset             -> filter_collector_process(Receiver_id, []);
    {filter, Msg}     -> Receiver_id ! {filter, append(List, Msg)},
      filter_collector_process(Receiver_id, append(List, Msg))
  end.

echo() ->
  receive
    stop -> ok;
    Msg -> io:format("Echo: ~p ~n" ,[Msg]), echo()
  end.

start() ->

  Echo = spawn(?MODULE, echo,[]),
  C = spawn(?MODULE, filter_collector_process, [Echo,[]]),
  P2 = spawn(?MODULE, filter_process,[C, 0]),

  P2!{filter,120},
  P2!{filter,109},
  P2!{filter,150},
  P2!{filter,101},
  P2!{filter,155},
  P2!{filter,114},
  P2!{filter,189},
  P2!{filter,114},
  P2!{filter,27},
  P2!{filter,121},
  P2!{filter,68},
  P2!{filter,32},
  P2!{filter,198},
  P2!{filter,99},
  P2!{filter,33},
  P2!{filter,104},
  P2!{filter,164},
  P2!{filter,114},
  P2!{filter,212},
  P2!{filter,105},
  P2!{filter,194},
  P2!{filter,115},
  P2!{filter,24},
  P2!{filter,116},
  P2!{filter,148},
  P2!{filter,109},
  P2!{filter,173},
  P2!{filter,97},
  P2!{filter,8},
  P2!{filter,115},
  P2!{filter,191},
  P2!{filter,33}.