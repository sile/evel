-module(evel_tests).

-include_lib("eunit/include/eunit.hrl").

elect_test_() ->
    {foreach,
     fun () ->
             {ok, Apps} = application:ensure_all_started(evel),
             Apps
     end,
     fun (Apps) ->
             lists:foreach(fun (A) -> application:stop(A) end, Apps)
     end,
     [
      {"Elects a leader",
       fun () ->
               Self = self(),
               ?assertMatch({Self, _}, evel:elect(foo, Self))
       end},
      {"Finds the leader",
       fun () ->
               ?assertEqual(error, evel:find_leader(foo)),

               Leader = evel:elect(foo, self()),
               ?assertEqual({ok, Leader}, evel:find_leader(foo))
       end},
      {"Dismiss the leader",
       fun () ->
               Leader = evel:elect(foo, self()),
               {ok, _} = evel:find_leader(foo),

               ?assertEqual(ok, evel:dismiss(Leader)),
               timer:sleep(10),
               ?assertEqual(error, evel:find_leader(foo))
       end}
     ]}.
