-module(bullyalgorithm).
-author("miwo").

-define(filterHigher, fun(Process) -> Process > self() end).
-define(filterLower, fun(Process) -> Process < self() end).
-define(filterTrue, fun(Process) -> Process == Process end).

-export([main/0, bully_node/4, bully_node_creator/2, message_counter/5]).


bully_node(ListOfProcesses, Counter, Coordinator, StartedElection) ->
  receive
    kill -> kill;
    startElection -> if
                       StartedElection == true ->
                         io:format("Hello I'm ~p i know: ~p, and I already started an Election... ~n", [self(), ListOfProcesses]),
                         bully_node(ListOfProcesses, Counter, Coordinator, StartedElection);
                       true ->
                         io:format("Hello I'm ~p i know: ~p, and I start an Election... ~n", [self(), ListOfProcesses]),
                         filter_send_message(ListOfProcesses, ?filterHigher, {election, self()}, Counter),
                         bully_node(ListOfProcesses, Counter, Coordinator, true)
                     end;
    {addNode, NewNode} -> %io:format("Hello I'm ~p i know: ~p and my new friend is: ~p ~n", [self(), ListOfProcesses, NewNode]),
      bully_node(lists:append(ListOfProcesses, [NewNode]), Counter, Coordinator, StartedElection);

    {coordinator, NewCoordinator} ->
      io:format("Hello I'm ~p i know: ~p, and the coordinator is: ~p. ~n", [self(), ListOfProcesses, NewCoordinator]),
      bully_node(ListOfProcesses, Counter, NewCoordinator, false);

    {election, PID} ->
      io:format("Hello I'm ~p i know: ~p, and I received election from: ~p. ~n", [self(), ListOfProcesses, PID]),
      filter_send_message([PID], ?filterTrue, {okay, self()}, Counter),
      self() ! startElection,
      bully_node(ListOfProcesses, Counter, Coordinator, StartedElection);


    {okay, PID} ->
      io:format("Hello I'm ~p I received an okay from ~p and I wait for the coordinator... \n", [self(), PID]),
      wait_for_coordinator_after_okay(ListOfProcesses, Counter, Coordinator, StartedElection)
  after 200 -> if
                 StartedElection == true ->
                   io:format("Hello I'm ~p I'm the Coordinator ~n", [self()]),
                   filter_send_message(ListOfProcesses, ?filterLower, {coordinator, self()}, Counter),
                   bully_node(ListOfProcesses, Counter, self(), false);

                 true -> %io:format("I'm ~p and I'm ready my coordinator is ~p my friends are ~p~n", [self(), Coordinator, ListOfProcesses]),
                   bully_node(ListOfProcesses, Counter, Coordinator, StartedElection)
               end
  end.



wait_for_coordinator_after_okay(ListOfProcesses, Counter, Coordinator, StartedElection) ->
  receive
    {coordinator, NewCoordinator} ->
      io:format("Hello I'm ~p i know: ~p, and the coordinator is: ~p. ~n", [self(), ListOfProcesses, NewCoordinator]),
      bully_node(ListOfProcesses, Counter, NewCoordinator, false);
    {okay, PID} ->
      io:format("Hello I'm ~p I received another okay from ~p and I wait for the coordinator... \n", [self(), PID]),
      wait_for_coordinator_after_okay(ListOfProcesses, Counter, Coordinator, StartedElection)

  after 300 ->
    io:format("Hello I'm ~p I didn't receive a new Coordinator from ~p I start new Election \n", [self(), ListOfProcesses]),
    self() ! startElection,
    bully_node(ListOfProcesses, Counter, Coordinator, StartedElection)
  end.



filter_send_message(ListOfProcesses, Pred, Message, Counter) ->
  List = lists:filter(Pred, ListOfProcesses),
  lists:foreach(fun(Process) -> Process ! Message, Counter ! Message end, List).



bully_node_creator(ListOfProcesses, Counter) ->
  Pid = spawn(?MODULE, bully_node, [ListOfProcesses, Counter, no_coordinator, false]),
  filter_send_message(lists:append(ListOfProcesses, [Pid]), ?filterTrue, {addNode, Pid}, Counter),
  Pid.


setup_n_bully_processes(Number, ListOfProcesses, _) when Number == 0 -> ListOfProcesses;

setup_n_bully_processes(Number, ListOfProcesses, Counter) ->
  Pid = bully_node_creator(ListOfProcesses, Counter),
  setup_n_bully_processes(Number - 1, lists:append(ListOfProcesses, [Pid]), Counter).


message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver) ->
  receive
    {coordinator, Pid} -> message_counter(ElectionMsg, OkayMsg, lists:append(CoordinatorMsg, [{coordinator, Pid}]), AddNodeMsg, Receiver);
    {okay, Pid} ->        message_counter(ElectionMsg, lists:append(OkayMsg, [{okay, Pid}]), CoordinatorMsg, AddNodeMsg, Receiver);
    {addNode, Pid} ->     message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, lists:append(AddNodeMsg, [{addNode, Pid}]), Receiver);
    {election, Pid} ->    message_counter(lists:append(ElectionMsg, [{election, Pid}]), OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver);

    {get, coordinator_messages} -> Receiver ! CoordinatorMsg, message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver);
    {get, okay_messages} -> Receiver ! OkayMsg, message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver);
    {get, addNode_messages} -> Receiver ! AddNodeMsg, message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver);
    {get, election_messages} -> Receiver ! ElectionMsg, message_counter(ElectionMsg, OkayMsg, CoordinatorMsg, AddNodeMsg, Receiver)
  end.

counter_creator(Receiver) ->
  spawn(?MODULE, message_counter, [[], [], [], [], Receiver]).

main() ->
  Counter = counter_creator(self()),
  List_of_Friends = setup_n_bully_processes(5, [], Counter),
  FirstElement = lists:nth(1, List_of_Friends),
  timer:sleep(100),
  FirstElement ! startElection,
  timer:sleep(2500),
  Counter  ! {get, election_messages},
  receive
    X -> io:format("Election-Messages sent: ~p ~nin Total: ~p election messages were sent ~n", [X, lists:flatlength(X)])
  end,
  io:format("Kill the process \n"),
  ThirdElement = lists:nth(5, List_of_Friends),
  ThirdElement ! kill,
  FirstElement ! startElection,
  timer:sleep(2500),
  Counter  ! {get, election_messages},
  receive
    XS -> io:format("Election-Messages sent: ~p ~nin Total: ~p election messages were sent ~n", [XS, lists:flatlength(XS)])
  end
.
