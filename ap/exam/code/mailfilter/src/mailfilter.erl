-module(mailfilter).

-behaviour(gen_server).

% API exports.
-export(
  [ start/1
  , stop/1
  , default/4
  , add_mail/2
  , get_config/1
  , enough/1
  , add_filter/4
  ]).

% gen_server mandated exports.
-export([ init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([safeMapsRemove/2
        ]).



%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-type mail()  :: any().
-type data()  :: any().
-type label() :: any().

-type result()          :: {done, data()} | inprogress.
-type labelled_result() :: {label(), result()}.
-type filter_result()   :: {just, data()}
                         | {transformed, mail()}
                         | {both, mail(), data()}
                         | unchanged.

-type filter_fun() :: fun((mail(), data()) -> filter_result()).

-type filter() :: {simple, filter_fun()}
                | {chain, list(filter())}
                | {group, list(filter()), merge_fun()}
                | {timelimit, timeout(), filter()}.

-type merge_fun() :: fun((list(filter_result() | inprogress)) ->
                               filter_result() | continue).

-type my_error() :: {error, any()} | {error, internal_err}.


-spec start(integer()) -> {ok, pid()} | my_error().
start(Cap) ->
  case gen_server:start(?MODULE, Cap, []) of
    {ok, MailServer} -> {ok, MailServer};
    {error, Reason}  -> {error, Reason};
    _                -> {error, internal_err}
  end.


-spec stop(any()) -> {ok, ext_state()} | my_error().
stop(MailServer) ->
  case gen_server:call(MailServer, stop) of
    {ok, State}     -> State;
    {error, Reason} -> {error, Reason};
    _               -> {error, internal_err}
  end.


-spec add_mail(pid(), mail()) -> {ok, ext_mail_ref()} | my_error().
add_mail(MS, Mail) ->
  case gen_server:call(MS, {add_mail, Mail}) of
    {ok, MailRef}   -> {ok, MailRef};
    {error, Reason} -> {error, Reason};
    _               -> {error, internal_err}
  end.


-spec get_config(ext_mail_ref()) -> {ok, config()} | my_error().
get_config({MailServer, MailTag}) ->
  case gen_server:call(MailServer, {get_config, MailTag}) of
    {ok, Config}    -> {ok, Config};
    {error, Reason} -> {error, Reason};
    _               -> {error, internal_err}
  end.


-spec default(pid(), label(), filter(), data()) -> any().
default(MailServer, Label, Filter, InitData) ->
  gen_server:cast(MailServer, {add_default, Label, Filter, InitData}).


-spec enough(ext_mail_ref()) -> any().
enough({MS, MailTag}) ->
  gen_server:cast(MS, {enough, MailTag}).


-spec add_filter(ext_mail_ref(), label(), filter(), data()) -> any().
add_filter({MailServer, MailTag}, Label, Filter, InitData) ->
  gen_server:cast(MailServer, {add_filter, MailTag, Label, Filter, InitData}).






%%%%%%%%%%%%%%%%%
%%% INTERNALS %%%
%%%%%%%%%%%%%%%%%
-type state() ::
  #{current  := integer(),            % current number of filters.
    capacity := integer() | infinite, % max filter capacity.

    coords   := coords(),  % mail ref -> coordinator   for this mail.
    configs  := configs(), % mail ref -> latest config for this mail.

    defaults := defaults() % default filters.
   }.

-type coords()   :: #{mail_tag() := pid()}.
-type configs()  :: #{mail_tag() := config()}.
-type defaults() :: #{label() := {filter(), data(), integer()}}.

-type mail_tag() :: reference().
-type ext_mail_ref() :: {pid(), mail_tag()}.


-type config() :: #{label() := result()}.
-type ext_state() :: list({mail(), list(labelled_result())}).



-spec handle_call(term(), {pid(), term()}, state()) ->
  {reply, {ok, term()}, state()} | {reply, {error, term()}, state()}.
handle_call({add_mail, Mail}, {_, Tag},
            #{configs := Configs, defaults := Defaults,
              current := Current} = State) ->


  Me = self(),
  MailTag = Tag, % TODO: different format?. Could also be make_ref().
                 % For now, just use Tag associated with sender's call.

  % spawn a new coordinator for this mail.
  case coordinator:start(Mail, MailTag, Defaults, Me) of
    {ok, Coord} ->

      State2 = add_coord(MailTag, Coord, State),
      DefaultsLabels = maps:keys(Defaults),

      % for each default filter, init emptry entry in the config for Mail.
      DefaultResults = maps:from_list(
                         lists:map(fun (Label) -> {Label, inprogress}
                                   end, DefaultsLabels)),
      % update configs and state.
      Configs2 = Configs#{MailTag => DefaultResults},
      NumNewFilters = default_filters_count(Defaults),
      State3 = State2#{configs => Configs2,
                       current => Current + NumNewFilters},

      {reply, {ok, {Me, MailTag}}, State3};

    {error, Reason} -> {reply, {error, Reason}, State};
    _               -> {reply, {error, coord_spawn_error}, State}
  end;


% Returns the latest known config for Mail. Does not prompt the given
% coordinator to send back updated config.
% TODO: is this assumption wrong/dangerous?
handle_call({get_config, MailTag}, _From,
            #{configs := Configs} = State) ->

  Reply =
    case safeMapsFind(MailTag, Configs) of
      {ok, Config} ->
        case Config of
          #{} = Config2 -> {ok, maps:to_list(Config2)};
          inprogress    -> {ok, inprogress};
          _Unexpected   -> {error, internal_err}
        end;

      lookup_fail  -> {error, label_unknown};
      InternalErr  -> InternalErr
    end,
  {reply, Reply, State};

handle_call(stop, From, #{configs := Configs} = State) ->
  gen_server:reply(From, {ok, lists:map(fun ({Tag, Config}) ->
                                        {Tag, maps:to_list(Config)}
                                   end, maps:to_list(Configs))}),
  {stop, {shutdown, normal}, State};

handle_call(_Msg, _From, State) ->
  {reply, {error, unrecognized_call}, State}.


-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({add_default, Label, Filter, InitData},
            #{defaults := Defaults} = State) ->

  case safeMapsFind(Label, Defaults) of

    lookup_fail -> % label does not exist? good, let's add it!
      Defaults2 = Defaults#{Label => {Filter,
                                      InitData,
                                      filter_count(Filter)}},
      {noreply, State#{defaults => Defaults2}};

    _ -> {noreply, State}
  end;


handle_cast({enough, MailTag}, #{coords   := Coords,
                                 current  := Current,
                                 defaults := Defaults} = State) ->

  case safeMapsFind(MailTag, Coords) of
    {ok, Coord} ->
      gen_server:stop(Coord),
      case safeMapsRemove(MailTag, Coords) of
        {ok, Coords2} ->

          NumFiltsRem = default_filters_count(Defaults), % remember to free up capacity !
          {noreply, State#{coords => Coords2,
                           current => Current - NumFiltsRem - 1}};

        _ -> {noreply, State} % should never match since we just looked up MailTag.

      end;
    _MailUnknown -> {noreply, State} % no coordinator for mail? consider it stopped already.
  end;

handle_cast({add_filter, MailTag, Label, Filter, InitData},
            #{coords   := Coords,
              capacity := Cap,
              current  := Current} = State) ->

  NumFilters = filter_count(Filter),
  RoomForFilter = (Current + NumFilters) =< Cap,

  FilterExists = stateFilterExists(State, MailTag, Label),
  DoAddFilter = RoomForFilter and (not FilterExists),

  if DoAddFilter ->
       case safeMapsFind(MailTag, Coords) of
         {ok, Coord} ->

           gen_server:cast(Coord, {add_filter, Label, Filter, InitData}),

           State2 = stateUpdateConfig(State, MailTag, Label, inprogress),

           Current2 = Current + NumFilters,

           {noreply, State2#{current => Current2}};

         _MailUnknown  -> {noreply, State}
       end;
     not DoAddFilter -> {noreply, State}
  end;


handle_cast({update_config, MailTag, New}, State) ->
  {noreply, stateSetConfig(State, MailTag, New)};

handle_cast({worker_shutdown, MailTag, Label},
            #{configs := Configs} = State) ->

  case safeMapsFind(MailTag, Configs) of
    {ok, Config} ->
      case safeMapsRemove(Label, Config) of
        {ok, Config2} ->
          Configs2 = Configs#{MailTag => Config2},
          {noreply, State#{configs => Configs2}};
        _ -> {noreply, State}
      end;
    _ -> {noreply, State}
  end;

handle_cast(_, State) -> {noreply, State}.


-spec init(integer()) -> {ok, state()}.
init(Capacity) ->
  InitState = #{current  => 0,
                capacity => Capacity,
                coords   => #{},
                configs  => #{},
                defaults => #{}
               },
  {ok, InitState}.

% TODO: cleanup necessary at this point..?
terminate(_Reason, _State) -> ok.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
add_coord(MailTag, Coord, #{coords := Coords} = State) ->
  State#{coords => Coords#{MailTag => Coord}}.


stateSetConfig(#{configs := Configs} = State,
               MailTag, New) ->
  State#{configs => Configs#{MailTag => New}}.

stateUpdateConfig(#{configs := Configs} = State,
                  MailTag, Label, New) ->
  case safeMapsFind(MailTag, Configs) of
    {ok, Config} ->

      Config2 = Config#{Label => New},
      State#{configs => Configs#{MailTag => Config2}};
    _ -> State
  end.


stateFilterExists(#{configs := Configs}, MailTag, Label) ->
  try safeMapsFind(MailTag, Configs) of
    {ok, Config} -> maps:is_key(Label, Config);
    _ -> false
  catch
    error:_ -> false % TODO: should probably return this explicitly.
  end.


filter_count({simple, _}) ->
  1;
filter_count({chain, Filters}) ->
  filter_list_count(Filters);

filter_count({group, Filters, _}) ->
  filter_list_count(Filters);

filter_count({timelimit, _, Filter}) ->
  filter_count(Filter).

filter_list_count(Filters) ->
  lists:sum(lists:map(fun (Filter) -> filter_count(Filter)
                      end, Filters)).

default_filters_count(Defaults) ->
  lists:sum(lists:map(fun ({_Filter, _Data, Count}) -> Count
                      end, maps:values(Defaults))).

%%%%%%%%%%%%%%%%%%%%
%%% MISC HELPERS %%%
%%%%%%%%%%%%%%%%%%%%
safeMapsFind(Key, Map) ->
  try maps:find(Key, Map) of
    {ok, Val} -> {ok, Val};
    _         -> lookup_fail
  catch
    error:Exception -> io:format(">> safeMapsFind() - caught: ~p~n~n",
                                 [Exception]),
                       {error, {internal_error, Exception}}
  end.

safeMapsRemove(Key, Map) ->
  try maps:remove(Key, Map) of
    Map2 -> {ok, Map2}
  catch
    _:_ -> {error, internal_error}
  end.
