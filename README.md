evel
=====

[![hex.pm version](https://img.shields.io/hexpm/v/evel.svg)](https://hex.pm/packages/evel)

`evel` is a distributed leader election library which has eventual consistency.

If the nodes which have started `evel` application eventually agree with the same member list (usually distributed erlang ensures it),
each election will elect a single leader who is recognized by all of them.

In the quiescent state (e.g., there are no joining nodes to the cluster),
the cost of an election is `O(M)` where `M` is the number of members (≈  nodes) who are interested in the election.
It is unaffected by the cluster size to which they belong.

`evel` provides weaker consistency than [global](http://erlang.org/doc/man/global.html) which can be regarded as OTP's leader election module.
But it is more scalable and has higher availability (e.g., more tolarent to timing failures of some nodes in the same cluster).

See [Comparison with the global module](#comparison-with-the-global-module) for more details.

Build
-----

To build the library for stand-alone usage:
```sh
$ git clone https://github.com/sile/evel.git
$ cd evel
$ ./rebar3 compile
$ ./rebar3 shell
$ > evel:module_info().
```

If you want to use from your application:
```erlang
%% In your 'rebar.config'

%% Add following lines
{deps,
 [
   evel
 ]}.
```

Example
-------

```erlang
$ ./rebar3 shell --sname master

%%
%% Starts 10 slave nodes on the local machine
%%
> ok = evel_debug:slave_start_link_n(10).
> nodes().
['1@localhost','2@localhost','3@localhost','4@localhost',
'5@localhost','6@localhost','7@localhost','8@localhost',
'9@localhost','10@localhost']

%%
%% Elects the leader from among following candidates
%%
> Candidates = [spawn(timer, sleep, [infinity]) || _ <- lists:seq(1, 100)].
> rpc:parallel_eval([{evel, elect, [foo, Candidate]} || Candidate <- Candidates]).

%%
%% Finds the elected leader
%%
> {ok, Leader} = evel:find_leader(foo).
> {Results0, []} = rpc:multicall(evel, find_leader, [foo]).
> lists:foreach(fun (R) -> {ok, Leader} = R end, Results0). % All nodes agree with the single leader

%%
%% Dismisses the leader
%%
> ok = evel:dismiss(Leader).
> error = evel:find_leader(foo).
> {Results1, []} = rpc:multicall(evel, find_leader, [foo]).
> lists:foreach(fun (R) -> error = R end, Results1).
```

API
---

See [EDoc Documents](doc/README.md)

Comparison with the global module
---------------------------------

The following examples depict some differences between `evel` and `global`.

```erlang
$ ./rebar3 shell --sname master
%%%
%%% SETUP
%%%

%% Starts 100 slave nodes on the local machine
> ok = evel_debug:slave_start_n(50).
> nodes().
['1@localhost','2@localhost','3@localhost','4@localhost',
'5@localhost','6@localhost','7@localhost','8@localhost'|...]

%% A auxiliary function
> RpcMultiApply = fun (F) -> {Result, []} = rpc:multicall(erlang, apply, [F, []]), Result end.


%%%
%%% Consistency
%%%

%% [global]
%% Every member always agree with a single leader.
> RpcMultiApply(fun () -> global:register_name(foo, spawn(timer, sleep, [infinity])), global:whereis_name(foo) end).
> lists:usort(v(-1)).
[<0.617.0>]

%% [evel]
%% At the time of conflict, multiple leaders may coexist temporarily.
> RpcMultiApply(fun () -> evel:elect(foo, spawn(timer, sleep, [infinity])) end).
> lists:usort(v(-1)).
[{<10721.296.0>,<10721.298.0>},
 {<10720.298.0>,<10720.300.0>},
 {<10729.307.0>,<10729.309.0>}]

%% But every member eventually agree with a single leader.
> RpcMultiApply(fun () -> evel:elect(foo, spawn(timer, sleep, [infinity])) end).
> lists:usort(v(-1)).
[{<10720.298.0>,<10720.300.0>}]


%%
%% Efficiency (or Scalability)
%%

%% [global]
%% All members participate the same election.
> timer:tc(fun () -> length(RpcMultiApply(fun () -> global:register_name(bar, spawn(timer, sleep, [infinity])) end)) end).
{7573849,51} % 7.573 seconds

%% Each member participate each election.
> timer:tc(fun () -> length(RpcMultiApply(fun () -> global:register_name(node(), spawn(timer, sleep, [infinity])) end)) end).
{9992855,51} % 9.992 seconds

%% [evel]
%% All members participate the same election.
> timer:tc(fun () -> length(RpcMultiApply(fun () -> evel:elect(bar, spawn(timer, sleep, [infinity])) end)) end).
{99444,51} % 0.099 seconds

%% Each member participate each election.
> timer:tc(fun () -> length(RpcMultiApply(fun () -> evel:elect(node(), spawn(timer, sleep, [infinity])) end)) end).
{324833,51} % 0.324 seconds


%%
%% Availability
%%

%% Suspends a slave node
> os:cmd("kill -19 ${A_SLAVE_OS_PID}").

%% global will block until the node is resumed.
> global:register_name(baz, self()). % Type `Ctrl+G => i => c` to break

%% evel is unaffected by the suspension.
> evel:elect(baz, self()).
{<0.715.0>,<0.718.0>}

%% Resumes the slave node
> os:cmd("kill -18 ${A_SLAVE_OS_PID}").


%%
%% Knowledge
%%

%% [global]
%% Every member locally knowns the results of all elections.
> global:registered_names().
['8@localhost','6@localhost','23@localhost','1@localhost',
'16@localhost','43@localhost','14@localhost','42@localhost' | ...]

%% [evel]
%% Each member locally knows the results of elections in which are interested.
> evel:known_leaders().
[{foo,{<10720.298.0>,<10720.300.0>}},
 {master@localhost,{<0.665.0>,<0.667.0>}},
 {bar,{<10735.305.0>,<10735.307.0>}}]

%% If the node want to know the result of other election,
%% it will fetch the result from some remote nodes.
> evel:find_leader('1@localhost').
{ok,{<10663.315.0>,<10663.317.0>}}

> evel:known_leaders().
[{'1@localhost',{<10663.315.0>,<10663.317.0>}},
 {foo,{<10720.298.0>,<10720.300.0>}},
 {master@localhost,{<0.665.0>,<0.667.0>}},
 {bar,{<10735.305.0>,<10735.307.0>}}]
```

License
-------

This library is released under the MIT License.
See the [LICENSE](LICENSE) file for full license information.
