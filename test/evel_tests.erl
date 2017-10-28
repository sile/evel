%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
-module(evel_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------
elect_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(evel), Apps end,
     fun (Apps) -> lists:foreach(fun (A) -> application:stop(A) end, Apps) end,
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
               ?assertEqual({ok, Leader}, evel:find_leader(foo)),
               ?assertEqual(self(), evel:get_winner(Leader))
       end},
      {"Dismiss the leader",
       fun () ->
               Leader = evel:elect(foo, self(), [{link, false}]),
               {ok, _} = evel:find_leader(foo),

               ?assertEqual(ok, evel:dismiss(Leader)),
               timer:sleep(10),
               ?assertEqual(error, evel:find_leader(foo))
       end}
     ]}.

concurrent_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(evel), Apps end,
     fun (Apps) -> lists:foreach(fun (A) -> application:stop(A) end, Apps) end,
     [
      {"Election conflict",
       fun () ->
               Concurrency = 500,
               Parent = self(),
               lists:foreach(
                 fun (I) ->
                         spawn_link(
                           fun () ->
                                   evel:elect(foo, self(), [{link, false}, {priority, -I}]),
                                   timer:sleep(100),
                                   Result = evel:find_leader(foo),
                                   Parent ! {'FIND', Result},
                                   timer:sleep(infinity)
                           end)
                 end,
                 lists:seq(1, Concurrency)),
               timer:sleep(100),

               %% Eventually all members agree with a sigle leader
               {ok, Leader} = evel:find_leader(foo),
               lists:foreach(
                 fun (_) ->
                         receive
                             {'FIND', Result} ->
                                 ?assertEqual({ok, Leader}, Result)
                         end
                 end,
                 lists:seq(1, Concurrency))
       end}
     ]}.

is_leader_test() ->
    ?assert(evel:is_leader({self(), self()})),
    ?assertNot(evel:is_leader(self())).
