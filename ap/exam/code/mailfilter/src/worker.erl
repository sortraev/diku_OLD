-module(worker).

-behaviour(gen_server).

% gen_server mandated exports.
-export([ start/5
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        , handle_continue/2
        ]).


%%%%%%%%%%%%%%%%%%%%%%
%%% MAILWORKER API %%%
%%%%%%%%%%%%%%%%%%%%%%
% start(Mail, Tag, Coordinator) ->
start(Mail, Label, Filter, InitData, Coordinator) ->
  gen_server:start_link(
    ?MODULE, {Mail, Label, Filter, InitData, Coordinator}, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAILWORKER INTERNALS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type mail()   :: mailfilter:mail().
-type label()  :: mailfilter:label().
-type data()   :: mailfilter:data().
-type filter() :: mailfilter:filter().
-type filter_result() :: mailfilter:filter_result().

-type worker_state() ::
  #{mail   := mail(),   % current state of mail.
    label  := label(),  % filter label.
    filter := filter(), % filter.
    data   := data(),   % current filter data.
    parent := pid()     % parent coord whom this worker reports back to.
   }.



-spec init({mail(), label(), filter(), data(), pid()})
  -> {ok, worker_state()}.
init({Mail, Label, Filter, InitData, Coordinator}) ->

  InitState = #{mail   => Mail,
                label  => Label,
                filter => Filter,
                data   => InitData,
                parent => Coordinator
               },
  {ok, InitState, {continue, do_filter}}.

handle_cast({invalidate, NewMail}, State) ->
  {noreply, State#{mail => NewMail}, {continue, do_filter}};

handle_cast(_, State) ->
  {noreply, State}.

handle_continue(do_filter, #{label  := Label,
                             mail   := Mail,
                             filter := Filter,
                             data   := Data,
                             parent := Parent} = State) ->
  try
    run_filter(Filter, Mail, Data)
  of
    FilterResult ->
      gen_server:cast(Parent, {update_config, Label, FilterResult}),
      {noreply, State}
  catch
    _:_ -> timer:sleep(1), % TODO: figure out why this sleep is necessary
                           %       when it obviously shouldn't be.
           {stop, {shutdown, normal}, State}
  end.

% ignore calls.
handle_call(_, _, State) ->
  {reply, {error, call_unexpected}, State}.

terminate(_Reason, #{parent := Parent, label := Label}) ->
  gen_server:cast(Parent, {shutdown, Label}).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FILTER EVALUATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
-spec run_filter(filter(), mail(), data()) -> filter_result().
run_filter({simple, FilterFun}, Mail, Data) ->
    FilterFun(Mail, Data);

run_filter({chain, Filters}, Mail0, Data0) ->
  InitAcc = {unchanged, Mail0, Data0},

  {FilterResult, _, DataResult} =
    lists:foldr(
      fun (Filter, {FilterResultAcc, MailAcc, DataAcc}) ->
        FilterResultAcc2 = combine_filter_results(FilterResultAcc,
                            run_filter(Filter, MailAcc, DataAcc)),

        {MailAcc2, DataAcc2} = update_accs(FilterResultAcc2, MailAcc, DataAcc),
        {FilterResultAcc2, MailAcc2, DataAcc2}
      end, InitAcc, Filters),

  case FilterResult of
    unchanged -> {unchanged, DataResult};
    _         -> FilterResult
  end;


% TODO: for the moment, this is not very parallel (or:
%       parallel with a very small degree of parallelism).
run_filter({group, Filters, MergeFun}, Mail0, Data0) ->
  FilterResults = lists:map(fun (Filter) ->
                              run_filter(Filter, Mail0, Data0)
                            end, Filters),
  MergeFun(FilterResults);


% TODO: not implemented! but I let it return unchanged for testing purposes.
run_filter({timelimit, _TimeOut, _Filter}, _Mail, _Data) ->
  unchanged;

run_filter(_, _, _) -> unchanged.


%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%
update_accs(FiltRes, Mail0, Data0) ->
  case FiltRes of
    {just, Data}        -> {Mail0, Data };
    {transformed, Mail} -> {Mail,  Data0};
    {both, Mail, Data}  -> {Mail,  Data };
    _Unchanged          -> {Mail0, Data0}
  end.

combine_filter_results(Acc1, Acc2) ->
  case {Acc1, Acc2} of
    {unchanged, _} -> Acc2;
    {_, unchanged} -> Acc1;

    {_,                {both, _, _}}     -> Acc2;
    {{just, _},        {just, _}}        -> Acc2;
    {{transformed, _}, {transformed, _}} -> Acc2;

    {{just, Data}, {transformed, Mail}}    -> {both, Mail, Data};
    {{transformed, Mail}, {just, Data}}    -> {both, Mail, Data};
    {{both, Mail, _}, {just, Data}}        -> {both, Mail, Data};
    {{both, _, Data}, {transformed, Mail}} -> {both, Mail, Data};

    _ -> throw(missing_combine_case)  % this should never match, but even if it did, it
                                      % would be preferable to discover it immediately,
                                      % ie. from a nasty exception.
  end.
