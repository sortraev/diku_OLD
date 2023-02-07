-module(coordinator).

-behaviour(gen_server).

% gen_server mandated exports.
-export([ start/4
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).


%%%%%%%%%%%%%%%%%%%%%%%
%%% COORDINATOR API %%%
%%%%%%%%%%%%%%%%%%%%%%%
start(Mail, MailTag, Defaults, MailServer) ->
  gen_server:start_link(?MODULE, {Mail, MailTag, Defaults, MailServer}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% COORDINATOR INTERNALS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type mail_tag() :: mailfilter:mail_tag().
-type mail()     :: mailfilter:mail().
-type label()    :: mailfilter:label().
-type data()     :: mailfilter:data().

-type filter()           :: mailfilter:filter().

-type flag()    :: valid | invalid.
-type flags()   :: #{label() := flag()}.
-type workers() :: #{label() := pid()}.
-type config()  :: mailfilter:config().


-type coord_state() ::
  #{orig    := mail(), % original mail.
    mail    := mail(), % current state of mail.

    workers := workers(), % workers, flags, and config map filter
    flags   := flags(),   % labels to worker servers, valid flags, and
    config  := config(),  % last config for these filters, respectively.

    parent  := pid(),     % mailfilter server whom this coord reports back to.
    tag     := mail_tag() % identifies this coord to the mailfilter server.
   }.


state_update_result(#{config := Config} = State, Label, New) ->
  State#{config => Config#{Label => New}}.


state_update_flag(#{flags := Flags} = State, Label, New) ->
  State#{flags => Flags#{Label => New}}.


invalidate_all_but(Label, #{workers := Workers,
                            flags   := Flags,
                            mail    := NewMail} = State) ->
  case safeMapsRemove(Label, Workers) of
    {ok, Workers2} ->
      maps:map(fun (_Label, Pid) ->
                   gen_server:cast(Pid, {invalidate, NewMail})
               end, Workers2),

      State#{flags => (maps:map(fun (_Label, _Flag) ->
                    invalid
                end, Flags))#{Label => valid}};
    _ -> State
  end.


handle_cast({worker_shutdown, Label},
            #{parent := Parent,
              tag    := MailTag} = State) ->
  gen_server:cast(Parent, {worker_shutdown, MailTag, Label}),
  {noreply, State};


handle_cast({update_config, Label, FilterResult},
            #{parent := Parent,
              tag    := MailTag} = State) ->
  State3 = #{config := Config2} =
    case FilterResult of

      {transformed, Mail} ->
        invalidate_all_but(Label, State#{mail => Mail});

      {both, Mail, Data} ->
        State2 = state_update_result(State, Label, {done, Data}),
        invalidate_all_but(Label, State2#{mail => Mail});

      {_JustOrUnchanged, Data} ->
        State2 = state_update_result(State, Label, {done, Data}),
        state_update_flag(State2, Label, valid);

      _Unexpected -> State
    end,

  case all_valid(State3) of
    true -> % notify parent of the new Config for this mail.
      gen_server:cast(Parent, {update_config, MailTag, Config2});

    _ -> nop % some workers need to recompute their filters first.
  end,

  {noreply, State3};


handle_cast({add_filter, Label, Filter, InitData},
            #{mail    := Mail,
              workers := Workers} = State) ->

  case spawn_worker(Mail, Label, Filter, InitData) of
    {ok, Worker} ->
      Workers2 = Workers#{Label => Worker},
      State2 = #{flags := Flags} = invalidate_all_but(Label, State#{mail => Mail}),
      Flags2 = Flags#{Label => invalid},
      {noreply, State2#{workers => Workers2, flags => Flags2}};

    _SpawnError -> {noreply, State}
  end.

handle_call(_, _, State) ->
  {reply, {error, call_unexpected}, State}.


-spec init({mail(), mail_tag(), #{label() := {filter(), data()}}, pid()})
  -> {ok, coord_state()}.
init({Mail, MailTag, Defaults, MailServer}) ->

  Workers =  spawn_default_filters(Mail, Defaults),
  Flags   =  init_flags(Defaults),
  Config  =  init_config(Defaults),

  InitState = #{orig => Mail,
                mail => Mail,

                workers => Workers,
                flags   => Flags,
                config  => Config,

                parent  => MailServer,
                tag     => MailTag
               },

  {ok, InitState}.

init_flags(Defaults) ->
  maps:map(fun(_, _) -> invalid end, Defaults).

init_config(Defaults) ->
  maps:map(fun(_Key, {_Filter, InitData, _}) -> InitData end, Defaults).


% TODO: cleanup necessary at this point?
terminate(_Reason, _State) -> ok.


%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
spawn_worker(Mail, Label, Filter, InitData) ->
  Me = self(),
  case worker:start(Mail, Label, Filter, InitData, Me)  of
    {ok, Worker}    -> {ok, Worker};
    {error, Reason} -> {error, Reason};
    _               -> {error, worker_spawn_error}
  end.


-spec spawn_default_filters(mail(), #{label() := {filter(), data(), integer()}})
        -> workers().
spawn_default_filters(Mail, Defaults) ->

  % PidsOrErrors is a map of type #{label() := (pid() | Error)}
  PidsOrErrors =
    maps:map(fun (Label, {Filter, InitData, _}) ->
                 case spawn_worker(Mail, Label, Filter, InitData) of
                 {ok, Coord} -> Coord;
                 _           -> error
                 end
             end, Defaults),

  % filter out any errors.
  maps:from_list(
    lists:filter(fun ({_Label, PidOrError}) ->
                       PidOrError =/= error
                 end, maps:to_list(PidsOrErrors))).

all_valid(#{flags := Flags}) ->
  lists:all(fun (Flag) -> Flag == valid end, maps:values(Flags)).

%%%%%%%%%%%%%%%%%%%%
%%% MISC HELPERS %%%
%%%%%%%%%%%%%%%%%%%%
safeMapsRemove(Key, Map) -> mailfilter:safeMapsRemove(Key, Map).
