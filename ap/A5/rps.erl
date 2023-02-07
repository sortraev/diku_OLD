-module(rps).
-export([start/0
         ,queue_up/3
         ,move/2
         ,statistics/1
         ,drain/3
         ,run/0
         ,setup/0
        ]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, terminate/2]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
start() ->
  gen_server:start(?MODULE, [], []).

queue_up(BrokerPid, Name, N) ->
  gen_server:call(BrokerPid, {queue_up, Name, N}, infinity).

statistics(BrokerPid) ->
  gen_server:call(BrokerPid, statistics).

drain(BrokerPid, Pid, Msg) ->
  gen_server:cast(BrokerPid, {drain, Pid, Msg}).

move(CoordRef, Choice) ->
  gen_statem:call(CoordRef, {move, Choice}).


%%%%%%%%%%%%%%%%%
%%% internals %%%
%%%%%%%%%%%%%%%%%
-record(player, {name :: string()
                ,n    :: integer()
                ,ref  :: gen_server:from()
                }).

init_state() -> {#{}, [], 0}.

init(_) ->
  State = init_state(),
  io:format("state: ~p~n~n", [State]),
  {ok, State}.

terminate(_Reason, _State) -> ok.


handle_call(statistics, _, {Queue, Coords, Longest} = State) ->
  InQueue = maps:size(Queue),
  Ongoing = lists:flatlength(Coords),
  {reply, {ok, Longest, InQueue, Ongoing}, State};


handle_call({queue_up, _, N}, _, State)
  when N =< 0 -> {reply, {error, negative_rounds_given}, State};


% assumes that for any N, there is at most one player queued up for a best-of-N
% game at any given point in time.
handle_call({queue_up, Name1, N}, From1, {Queue, Coords, L} = State) ->

  % create new player record and queue em up!
  P1 = #player{name = Name1, n = N, ref = From1},

  case safeMapsFind(N, Queue) of

    % no opponent ready: add Player1 to queue; leave them hanging for now.
    lookup_fail ->
      Queue2 = Queue#{N => P1},
      {noreply, {Queue2, Coords, L}};
      

    % Player2 ready.
    {ok, #player{name = Name2, ref = From2} = Player2} ->

      % Setup game and notify players!
      case coord:start(N, From1, From2) of % cannot fail.
        {ok, CoordRef} ->

          gen_server:reply(From1, {ok, Name2, CoordRef}), % cannot fail.
          gen_server:reply(From2, {ok, Name1, CoordRef}), % cannot fail.

          % dequeue Player2.
          case safeMapsRemove(Player2, Queue) of
            {ok, Queue2} -> {noreply, {Queue2, [CoordRef | Coords], L}};
            InternalErr  -> {reply, InternalErr, State}
          end;

        % since coord:start() can also return error and 'ignore'.
        {error, Reason} -> {reply, {error, Reason}, State};
        _ -> {reply, {error, coordinator_spawn_error}, State}
      end;


    % if some internal error occurs (probably a badmap exception),
    % notify player immediately.
    InternalErr -> {reply, InternalErr, State}
  end.



handle_cast({drain, Pid, Msg}, {Queue, Coords, _}) ->

  % notify all queued players that the server is stopping.
  maps:map(fun (_, #player{ref = QueuedPid}) ->
               gen_server:reply(QueuedPid, server_stopping) % cannot fail.
           end, Queue),

  % gracefully halt all coordinators.
  lists:foreach(fun (Coord) ->
                    gen_statem:cast(Coord, drain)           % cannot fail.
                end, Coords),

  if Pid /= none -> Pid ! Msg
  end,
  {stop, normal, init_state()}.



%%%%%%%%%%%%%%%
%%% helpers %%%
%%%%%%%%%%%%%%%
safeMapsFind(Key, Map) ->
  try maps:find(Key, Map) of
    {ok, Val} -> {ok, Val};
    _         -> lookup_fail
  catch
    Exception -> {error, {internal_error, Exception}}
  end.


safeMapsRemove(Key, Map) ->
  try maps:remove(Key, Map) of
    NewMap    -> {ok, NewMap}
  catch
    Exception -> {error, {internal_error, Exception}}
  end.



run() ->
  {ok, S} = setup(),
  spawn(fun () -> Foo = queue_up(S, sortraaaev, 3),
                  io:format("sortraev: ~p~n~n", [Foo]) end),
  spawn(fun () -> Foo = queue_up(S, psyko_malthe, 7),
                  io:format("psyko_malthe: ~p~n~n", [Foo]) end),
  S.


setup() ->
  {ok, S} = rps:start(),
  _B = spawn(fun () -> rps:queue_up(S, fedeJeppe17, 8) end),
  _C = spawn(fun () -> {ok, _, Coord} = rps:queue_up(S, psyko_malthe, 3),
                      rps:move(Coord, paper), rps:move(Coord, rock) end),
  {ok, S}.
