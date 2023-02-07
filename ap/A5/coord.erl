-module(coord).

-export([start/3,
         callback_mode/0, init/1,
         first_move/3, second_move/3]).

-type move() :: none | rock | paper | scissors.

-record(player, {pid  = none :: none | pid()
                ,ref  = none :: none | term()
                ,wins = 0    :: integer()
                ,move = none :: move()
                }).

-record(data, {n  :: integer() % rounds.
              ,p1 :: #player{} % player 1.
              ,p2 :: #player{} % player 2.
              ,played = 0 :: integer() % number of rounds played.
              }).


%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_statem stuff %%%
%%%%%%%%%%%%%%%%%%%%%%%%
callback_mode() -> state_functions.

start(N, From1, From2) ->
    gen_statem:start_link(?MODULE, {N, From1, From2}, []).

init({N, {Pid1, _}, {Pid2, _}}) ->

  InitData = #data{n  = N,
                   p1 = #player{pid = Pid1},
                   p2 = #player{pid = Pid2}},
  {ok, first_move, InitData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% state callback functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


on_drain(#data{p1 = #player{pid = Pid1, ref = Ref1},
                     p2 = #player{pid = Pid2, ref = Ref2}}) ->
  case Ref1 of
    none -> nop;
    Ref1 -> gen_statem:reply({Pid1, Ref1}, server_stopping) % cannot fail.
  end,
  case Ref2 of
    none -> nop;
    Ref2 -> gen_statem:reply({Pid2, Ref2}, server_stopping) % cannot fail.
  end,
  {stop, {shutdown, server_stopping}}.


first_move(cast, drain, Data) -> on_drain(Data);

first_move({call, From}, {move, Choice}, Data) ->
  case update_moves(From, Choice, Data) of
    {ok, Data2} -> {next_state, second_move, Data2};
    Error       -> {keep_state, Data, {reply, Error}}
  end.


second_move(cast, drain, Data) -> on_drain(Data);

second_move({call, From}, {move, Choice}, Data) ->

  case update_moves(From, Choice, Data) of
    {ok, Data2} ->

      % who won ?!?!
      Winner = who_won(Data),

      % prepare next round by resetting player moves and incrementing Played count.
      Data2 = prepare_next_round(award_points(Winner, Data)),

      % is the game over?
      case is_game_over(Data2) of
        true -> {stop_and_reply, {shutdown, game_over}, replies_game_over(Data2)};
        _    -> {next_state, first_move, Data2, replies_round_over(Winner, Data2)}
      end;

    % invalid move; keep state, but reply with an error.
    Error       -> {keep_state, Data, {reply, Error}}
  end.



%%%%%%%%%%%%%%%
%%% helpers %%%
%%%%%%%%%%%%%%%

% valid_move(Choice) ->
%   (Choice == rock) or (Choice == paper) or (Choice == scissors).

% assumes that it is called *after* Choice has been validated.
% takes the Ref associated with Choice, since this will be
% needed for a later reply.
set_move(Choice, Ref, Player) ->
  Player#player{move = Choice, ref = Ref}.

update_moves({Pid, _}, _, #data{p1 = #player{pid = Pid1},
                                p2 = #player{pid = Pid2}})
  when (Pid /= Pid1) and (Pid /= Pid2) -> {error, unauthorized_player};

update_moves(_From, Choice, _Data)
  when ((Choice /= rock)  and
        (Choice /= paper) and
        (Choice /= scissors)) -> {error, bad_move};

% player and move have both been validated. go ahead and update moves.
update_moves({Pid, Ref}, Choice, 
             #data{p1 = #player{pid = Pid1} = P1,
                   p2 = #player{pid = Pid2} = P2} = Data) ->
  case Pid of
    Pid1 -> {ok, Data#data{p1 = set_move(Choice, Ref, P1)}};
    Pid2 -> {ok, Data#data{p2 = set_move(Choice, Ref, P2)}}
  end.



who_won(#data{p1 = #player{move = M1}, p2 = #player{move = M2}}) ->
  case {M1, M2} of
    {rock,     scissors} -> p1;
    {paper,    rock    } -> p1;
    {scissors, paper   } -> p1;
    {rock,     paper   } -> p2;
    {paper,    scissors} -> p2;
    {scissors, rock    } -> p2;
    _                    -> none
  end.



% increment_wins/3 assumes that it is only called by 
% award_points/2, and thus no error checking necessary.
increment_wins(#player{wins = Wins} = Player) ->
  Player#player{wins = Wins + 1}.

% assumes that Winner is an atom returned by who_won/1.
award_points(Winner, #data{p1 = P1, p2 = P2} = Data) ->
  case Winner of
    p1   -> Data#data{p1 = increment_wins(P1)};
    p2   -> Data#data{p2 = increment_wins(P2)};
    none -> Data
  end;

award_points(_, _) -> {error, {internal_error, bad_data}}.



prepare_next_round(#data{p1 = P1, p2 = P2, played = Played} = Data) ->
  Data#data{p1 = P1#player{move = none, ref = none},
            p2 = P2#player{move = none, ref = none},
            played = Played + 1};

prepare_next_round(_) -> {error, {internal_error, bad_data}}.



is_game_over(#data{n      = N,
                   played = Played,
                   p1 = #player{wins = W1},
                   p2 = #player{wins = W2}}) ->
  (Played >= N) or (W1 > N/2) or (W2 > N/2);

is_game_over(_) -> {error, {internal_error, bad_data}}.




%%%%%%%%%%%%%%%%%%%%%%%
%%% player replying %%%
%%%%%%%%%%%%%%%%%%%%%%%

% TODO: all of these are missing some sort of error checking.

make_replies({Reply1, Reply2},
             #data{p1 = #player{pid = Pid1, ref = Ref1},
                   p2 = #player{pid = Pid2, ref = Ref2}}) ->
  From1 = {Pid1, Ref1},
  From2 = {Pid2, Ref2},
  [{reply, From1, Reply1}, {reply, From2, Reply2}].

replies_round_over(Winner, Data) ->
  Replies =
    case Winner of
      p1 -> {round_won, round_lost};
      p2 -> {round_lost, round_won};
      _  -> {tie, tie}
    end,
  make_replies(Replies, Data).

replies_game_over(#data{p1 = #player{wins = W1},
                        p2 = #player{wins = W2}} = Data) ->
  make_replies({{game_over, W1, W2}, {game_over, W2, W1}}, Data).
