-module(test_mailfilter).

-include_lib("eunit/include/eunit.hrl").

-export([test_all/0, test_everything/0]).
-export([test_mailfilter/0]). % Remember to export the other function from Q2.2


test_everything() ->
  test_all().

test_all() ->
  test_mailfilter().

test_mailfilter() ->
  Capacity = 17,

  {ok, MS} = mailfilter:start(Capacity),

  Mail1 = 5,
  Mail2 = 19,

  % adding two default filters, which together take up 14 capacity.
  mailfilter:default(MS, foo, nested_chain_filter(), 7),
  % mailfilter:default(MS, bar, nested_chain_filter(), 5),

  {ok, MR1} = mailfilter:add_mail(MS, Mail1),
  {ok, MR2} = mailfilter:add_mail(MS, Mail2),

  % this filter will ask for 7 capacity, and will thus be rejected by the mail server.  
  mailfilter:add_filter(MR2, bar, nested_chain_filter(), 4),
                                                           
  timer:sleep(50),

  {ok, Conf1} = mailfilter:get_config(MR1),
  {ok, Conf2} = mailfilter:get_config(MR2),
  ?assertEqual(lists:flatlength(Conf1), 1),
  ?assertEqual(lists:flatlength(Conf2), 1),

  mailfilter:enough(MR1), % kill filter for MR1, freeing up 7 capacity.

  timer:sleep(50),

  % there should now be room for the new chain filter of size 7.
  mailfilter:add_filter(MR2, baz, nested_chain_filter(), 2),

  timer:sleep(50),

  {ok, Conf3} = mailfilter:get_config(MR2),

  % if the config for MR2 is now 2 elements long, 
  ?assertEqual(lists:flatlength(Conf3), 2),

  mailfilter:stop(MS).


decrement(Mail, MyCount) ->
  if Mail =< 0 -> {just, MyCount};
     Mail >  0 -> {both, Mail - 1, MyCount + 1}
  end.

simple_filter() -> {simple, fun decrement/2}.

chain_filter() ->
  {chain, [simple_filter(), simple_filter(), simple_filter()]}.

nested_chain_filter() ->
  {chain, [simple_filter(), chain_filter(), chain_filter()]}.
