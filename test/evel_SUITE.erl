%% Copyright (c) 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
-module(evel_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_testcase/1]).
-export([elect_test/1]).
-export([remote_elect_test/1]).
-export([dismiss_test/1]).
-export([parallel_elect_test/1]).
-export([nodes_join_test/1]).
-export([nodes_leave_test/1]).
-export([summary_test/1]).

-define(INITIAL_NODES, 6).
-define(SLAVE_WAIT, timer:sleep(1000)).

all() ->
    [
     elect_test,
     dismiss_test,
     remote_elect_test,
     parallel_elect_test,
     nodes_join_test,
     nodes_leave_test,
     summary_test
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(evel),
    {ok, _} = net_kernel:start([evel_ct, shortnames]),
    ok = evel_debug:slave_start_n(?INITIAL_NODES),
    ?SLAVE_WAIT,
    Config.

end_per_testcase(Config) ->
    ok = lists:foreach(fun ({_, Leader}) -> evel:dismiss(Leader) end, evel:known_leaders()),
    Config.

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------
elect_test(_Config) ->
    Self = self(),
    {Self, _} = Leader = evel:elect(foo, Self),
    {ok, Leader} = evel:find_leader(foo).

dismiss_test(_Config) ->
    Leader = evel:elect(foo, self()),
    {ok, Leader} = evel:find_leader(foo),
    ok = evel:dismiss(Leader, [{unlink, true}]),
    timer:sleep(10),
    error = evel:find_leader(foo).

remote_elect_test(_Config) ->
    Self = self(),
    {Self, _} = Leader = rpc:call(hd(nodes()), evel, elect, [foo, Self]),

    {ok, Leader} = evel:find_leader(foo),
    Leader = evel:elect(foo, spawn(timer, sleep, [1000])).

parallel_elect_test(_Config) ->
    Self = self(),
    _ = rpc:multicall(evel, elect, [foo, Self, [{link, false}]]),

    {ok, Leader} = evel:find_leader(foo),
    {Self, _} = Leader,

    {Results, []} = rpc:multicall(evel, find_leader, [foo]),
    ok = lists:foreach(fun (Result) -> {ok, Leader} = Result end, Results).

nodes_join_test(_Config) ->
    Self = self(),
    {Self, _} = Leader = evel:elect(foo, Self),
    ok = evel_debug:slave_start_link_n(?INITIAL_NODES + 1, ?INITIAL_NODES + 3),
    ?SLAVE_WAIT,
    ?INITIAL_NODES = length(nodes()) - 3,

    {Results, []} = rpc:multicall(evel, find_leader, [foo]),
    ok = lists:foreach(fun (Result) -> {ok, Leader} = Result end, Results).

nodes_leave_test(_Config) ->
    Self = self(),
    {Self, _} = Leader = evel:elect(foo, Self),
    Pid = spawn(fun () ->
                        evel_debug:slave_start_link_n(?INITIAL_NODES + 1, ?INITIAL_NODES + 3),
                        timer:sleep(infinity)
                end),
    ?SLAVE_WAIT,
    ?INITIAL_NODES = length(nodes()) - 3,

    {Results0, []} = rpc:multicall(evel, find_leader, [foo]),
    ok = lists:foreach(fun (Result) -> {ok, Leader} = Result end, Results0),

    true = exit(Pid, kill),
    ?SLAVE_WAIT,
    ?INITIAL_NODES = length(nodes()),

    {Results1, []} = rpc:multicall(evel, find_leader, [foo]),
    ok = lists:foreach(fun (Result) -> {ok, Leader} = Result end, Results1).

summary_test(_Config) ->
    _ = evel:elect(foo, self()),
    #{knowns := _, votes := _, people := _, agents := _} = evel_debug:summary().
