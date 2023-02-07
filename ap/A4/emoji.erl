-module(emoji).

-export([start/1, stop/1,
         new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3
        ]).



%%%%%%%%%%%%%%%
%%%   API   %%%
%%%%%%%%%%%%%%%
start(InitShorts) -> emojiServerStart(InitShorts).

new_shortcode(E, Short, Emoji) ->
  query(E, {new_shortcode, Short, Emoji}).

alias(E, Short1, Short2) ->
  query(E, {alias, Short1, Short2}).

delete(E, Short) ->
  query(E, {delete, Short}).

lookup(E, Short) ->
  query(E, {lookup, Short}).

analytics(E, AttachTo, F, FLabel, FInitState) ->
  query(E, {set_analytics, AttachTo, F, FLabel, FInitState}).

get_analytics(E, Short) ->
  query(E, {get_analytics, Short}).

remove_analytics(E, Short, FLabel) ->
  query(E, {rem_analytics, Short, FLabel}).

stop(E) ->
  query(E, stop).

all(E) ->
  query(E, all).

workers(E) ->
  query(E, workers).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%   Query handling   %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handleQuery(Query, {Emos, AFMap}=State) ->

  case Query of

    all     -> {Emos,  State};
    workers -> {AFMap, State};
    stop ->
      Res = haltAllAnalytics(AFMap),
      {Res, {#{}, #{}}};

    {new_shortcode, NewShort, NewEmoji} ->
      {Res, Emos2} = newShortcode(Emos, NewShort, NewEmoji),
      {Res, {Emos2, AFMap}};

    {lookup, Short} ->

      % TODO: handle errors from updateAnalytics.
      updateAnalytics(State, Short),
      {lookupEmoji(Emos, Short), State};

    {alias, Short1, Short2} ->
      {Res, Emos2} = addAlias(Emos, Short1, Short2),
      {Res, {Emos2, AFMap}};

    {delete, Short} ->
      deleteEmoji(State, Short);

    {set_analytics, Short, F, FLabel, FInitState} ->
      setAnalytics(State, Short, FLabel, F, FInitState);

    {rem_analytics, Short, FLabel} ->
      removeAnalytics(State, Short, FLabel);

    {get_analytics, Short} ->
      getAnalytics(State, Short);

    _ -> {{error, bad_query}, State}
  end.


% Add a new shortcode NewShort with associated emoji NewEmoji to Emos. Error if
% a record for NewShort already exists in Emos.
newShortcode(Emos, NewShort, NewEmoji) ->
  case emoExists(Emos, NewShort) of
    false -> {ok, Emos#{NewShort => NewEmoji}};
    true  -> {{error, short_already_bound}, Emos}
  end.

% Lookup record for Short in Emos and extract its associated Emoji.
% Error if no such record exists.
lookupEmoji(Emos, Short) ->
  case lookupEmo(Emos, Short) of
    {ok, E} -> {ok, E};
    _LookupError -> no_emoji
  end.


addAlias(Emos, Short1, Short2) ->
  case lookupEmo(Emos, Short1) of
    {ok, Emoji} ->

      case emoExists(Emos, Short2) of
        false -> {ok, Emos#{Short2 => Emoji}};
        true  -> {{error, alias_already_bound}, Emos} % alias already exists!
      end;

    Error -> {Error, Emos} % nothing to alias!
  end.


% If Emos contains a record for the short Short, then deleteEmoji deletes that
% record along with records for all other records with the same Emoji.

% RETURNS: {ok, Emos2}, where Emos2 is an updated list of
%          records, or original list if no record of Short.
deleteEmoji({Emos, AFMap} = State, Short) ->

  case lookupEmo(Emos, Short) of
    {ok, Emo} ->
      {HaltStatuses, AFMap2} =

        % FIXME: probably need to handle the case where AFMap is not a map.
        case safeFind(Emo, AFMap) of
          {ok, WorkersMap} ->
            Responses = haltAnalytics(WorkersMap),
            {Responses, maps:remove(Emo, AFMap)};

          lookup_fail   -> {ok, AFMap};
          InternalError -> {InternalError, AFMap}
        end,

      Status = case HaltStatuses of
                 {error, Failures} -> {warn, {failure, Failures}};
                 _ -> ok
               end,

      Emos2 = maps:filter(fun (_, Emo2) -> Emo2 /= Emo end, Emos),
      {Status, {Emos2, AFMap2}};

    {_, short_unknown} -> {{warn, delete_no_candidate}, State};
    InternalError -> {InternalError, State}
  end.


setAnalytics({Emos, AFMap} = State, Short, Label, F, FInitState) ->
  case lookupEmo(Emos, Short) of
    {ok, Emo} ->

      try maps:get(Emo, AFMap, #{}) of
        Workers ->

          % is a function with label Label already registered?
          case safeFind(Label, Workers) of

            {ok, _} -> {{error, analytics_already_registered}, State};

            lookup_fail -> % nope - spawn a worker and register the function!
              Me = self(),
              Worker = spawn(fun () -> workerLoop(Me, F, FInitState) end),
              Workers2 = Workers#{Label => Worker},

              {ok, {Emos, AFMap#{Emo => Workers2}}};

            InternalError -> InternalError

          end

      catch
        InternalError -> InternalError
      end;

    {_, short_unknown} -> {{error, analytics_attachee_unknown}, State};
    InternalError -> InternalError
  end.


removeAnalytics({Emos, AFs} = State, Short, FLabel) ->

  case lookupEmo(Emos, Short) of
    {ok, Emo} ->
      case safeFind(Emo, AFs) of
        {ok, Workers} ->
          case safeFind(FLabel, Workers) of
            {ok, Worker} ->

              % update analytics function registry; then halt associated worker.
              AFs2     = AFs#{Emo => maps:remove(FLabel, Workers)},
              Response = haltWorkerGetResponse(Worker),
              {Response, {Emos, AFs2}};

            lookup_fail   -> {{warn, bad_analytics_label, FLabel}, State};
            InternalError -> {InternalError, State}
          end;

        lookup_fail   -> {{warn, no_analytics_for_emoji, Emo}, State};
        InternalError -> {InternalError, State}
      end;

    AnyError -> {AnyError, State}
  end.


getAnalytics({Emos, AFMap} = State, Short) ->
  case lookupEmo(Emos, Short) of
    {ok, Emo} ->
      Result =
        case safeFind(Emo, AFMap) of
          {ok, WorkersMap} ->
            Me = self(),
            Results =
              lists:map(fun ({FLabel, Worker}) ->
                Worker ! {Me, get_state},
                Response = receive
                             {Worker, S} -> S
                           after 100 ->
                              worker_timeout
                           end,

                {FLabel, Response} end,
                maps:to_list(WorkersMap)),


            Status = filterFailures(Results),
            case Status of
              ok        -> {ok, Results};
              _AnyError -> {ok, Status}
            end;


          lookup_fail   -> {ok, []};
          InternalError -> InternalError
        end,
      {Result, State};

    AnyError -> {AnyError, State}
  end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Analytics worker related   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
workerLoop(E, F, S) ->

  Me = self(),
  receive
    {E, stop} ->
      E ! {Me, ok};

    {E, update_state, Short} ->
      S2 = F(Short, S),
      workerLoop(E, F, S2);

    {E, get_state} ->
      E ! {Me, S},
      workerLoop(E, F, S)
  end.


haltWorkerGetResponse(Worker) ->
  Worker ! {self(), stop},
  receive
    {Worker, Msg} -> Msg

  after 100 ->
    exit(Worker, kill), % TODO: yeeaah, I know ..
    worker_timeout
  end.


% Prompt a worker process to halt, wait 1/10 second for its response and return
% this, or a message signifying a worker timeout.
updateAnalytics({Emos, AFs} = _State, Short) ->

  case lookupEmo(Emos, Short) of
    {ok, Emo} ->

      case safeFind(Emo, AFs) of

        {ok, Workers} ->
          Me = self(),

          lists:foreach(fun (WorkerPid) ->
                             WorkerPid ! {Me, update_state, Short} end,
                       maps:values(Workers)),
          ok;
        lookup_fail   -> ok;     % no analytics attached? ay-ok!
        InternalError -> InternalError
      end;

    Error -> Error
  end.


% if Workers maps FLabels to worker PIDs, then haltWorkers(Workers)
% halts the worker process for each FLabel in Workers, storing their
% response in a list.
haltAnalytics(WorkersMap) ->

  Workers = maps:to_list(WorkersMap),

  % For each analytic attached to this particular
  % emoji, and its responsible worker ...
  WorkerResponses = lists:map(fun ({AF, Worker}) ->
                      % ... halt that worker and get its response.
                      {AF, haltWorkerGetResponse(Worker)}
                    end, Workers),

  % Remove ok's, since we only really care about failed halts; return an empty
  % map since all work
  filterFailures(WorkerResponses).


% AFMap is a map of maps of type #{Short => #{FLabel => Worker}}.
% AFMap maps shortcodes to maps, which in turn map analytic function labels to
% process ID's of processes running that particular analytic function.
haltAllAnalytics(AFMap) ->

  AFList = maps:to_list(AFMap),

  % Halt workers and fetch all of their responses.
  WorkerResponses =

    % For each short with registered analytics, and its responsible workers ...
    lists:map(fun ({Emo, Workers}) ->

      % Halt all workers associated with this Emoji.
      Responses = haltAnalytics(Workers),
      {Emo, Responses} end,
      AFList),

  % At this point, Responses is a list of tuples [{Emoji, Response}],
  % where Response is either `ok`, or a list of tuples {FLabel, Response2},
  % where Response2 is the reason for failure of the function with label FLabel.

  % return ok if all responses are ok, else a list of lists of failures for each
  % analytic function attached to each Emoji
  filterFailures(WorkerResponses).





%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

% True if a record with shortcode Short exists in Emos, else false.
% Success is assumed.
emoExists(Emos, Short) ->
  maps:is_key(Short, Emos).

safeFind(Key, Map) ->
  try maps:find(Key, Map) of
    {ok, Val} -> {ok, Val};
    _         -> lookup_fail
  catch
    Exception -> {error, {internal_error, Exception}}
  end.

% safeFind but for Emojis
lookupEmo(Emos, Short) ->
  case safeFind(Short, Emos) of
    {ok, Emo}     -> {ok, Emo};
    lookup_fail   -> {error, short_unknown};
    InternalError -> InternalError
  end.


% returns ok if all elements of Statuses are ok;
% else an error with the list of failures.
filterFailures(Statuses) ->
  Failures =
    lists:filter(fun ({_, Status}) ->
      case Status of
        {error, _}     -> true;
        worker_timeout -> true;
        _ -> false
      end end, Statuses),

  case Failures of
    [] -> ok;   % no failures? awesome!
    _ -> {error, Failures}
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Client/server internals (mostly a copy+paste of Ken's)   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query(E, Query) ->
  Ref = make_ref(),
  E ! {self(), Ref, Query},

  receive
    {Ref, Response} -> Response

  after 1000 -> % 1 second response timeout because why not.
    io:format("Server timeout!~n")
  end.

% Maps each {Short, Emoji} record in the initial input list to their internal
% record representation; spawns a server loop and links a supervisor.
emojiServerStart(InitEmos) ->
  {ok, spawn(fun () ->
    process_flag(trap_exit, true),

    % setup initial state

    InitState = {maps:from_list(InitEmos), #{}},

    Supervisor = self(),
    Worker = spawn_link(fun () -> emojiServerLoop(Supervisor, InitState) end),
    emojiServerSupervisor(Worker, nothing, InitState)
    end)}.


emojiServerSupervisor(Worker, LastReceived, LastGoodState) ->
  receive

    % user called stop(); gracefully exit.
    % TODO: should also prompt analytic workers a graceful exit.
    {'EXIT', Worker, graceful_exit} ->
      io:format("~p exited gracefully!~n", [Worker]),
      ok;

    % worker crashed. copy+pasted from Ken's `keep_alive_revisited.erl`.
    {'EXIT', Worker, Reason} ->
      io:format("~p exited because of ~p~n", [Worker, Reason]),

      Supervisor = self(),
      Worker2 = spawn_link(fun () -> emojiServerLoop(Supervisor, LastGoodState) end),
      case LastReceived of
        {Client, Ref, _Res} -> Client ! {Ref, {error, Reason}};
        _ -> nothing
      end,
      emojiServerSupervisor(Worker2, nothing, LastGoodState);

    % backup state.
    {Worker, backup_state, State} ->
      emojiServerSupervisor(Worker, LastReceived, State);

    % message from client.
    Message ->
      Worker ! Message,
      emojiServerSupervisor(Worker, Message, LastGoodState)

  end.

emojiServerLoop(Supervisor, State) ->

  Supervisor ! {self(), backup_state, State},

  receive

    {Sender, Ref, stop} ->
      {Response, _} = handleQuery(stop, State),
      Sender ! {Ref, Response},
      exit(graceful_exit);

    {Sender, Ref, Query} ->
      {Response, NewState} = handleQuery(Query, State),
      Sender ! {Ref, Response},
      emojiServerLoop(Supervisor, NewState);

    Message -> io:format(">> Server: received something, but format "
                         "wrong and cannot discern sender.~n"
                         "Message: ~p~n", [Message]),
         emojiServerLoop(Supervisor, State)
  end.
