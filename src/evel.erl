%% @copyright 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% This software is released under the MIT License.
%% See the LICENSE file in the project root for full license information.
%%
%% @doc An Eventual Leader Election Library
%%
%% This module provides functionality to elect the leader
%% which will be eventually agreed by all member of the same distributed erlang cluster.
%%
%% ```
%% %%
%% %% Elects the leader
%% %%
%% > Leader = evel:elect(foo, self()).
%%
%% %%
%% %% Finds the leader of an election
%% %%
%% > {ok, Leader} = evel:find_leader(foo).
%% > error = evel:find_leader(bar).
%%
%% %%
%% %% Dismisses the leader
%% %%
%% > ok = evel:dismiss(foo).
%% > error = evel:find_leader(foo).
%% '''
%%
%% @end
-module(evel).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([elect/2, elect/3]).
-export([dismiss/1, dismiss/2]).
-export([find_leader/1, find_leader/2]).
-export([known_leaders/0]).
-export([is_leader/1]).
-export([get_winner/1]).
-export([get_certificate/1]).

-export_type([election_id/0]).
-export_type([candidate/0]).
-export_type([leader/0]).
-export_type([winner/0]).
-export_type([certificate/0]).
-export_type([elect_option/0]).
-export_type([find_option/0]).
-export_type([dismiss_option/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type election_id() :: term().
%% The identifier of an election.
%% In each election, only one leader is elected.

-type candidate() :: pid().
%% A candidate of an election.

-type winner() :: candidate().
%% The winner of an election.

-type certificate() :: pid().
%% The certificate to gurantee the legitimacy of a leader.
%%
%% If the certificate process is down, the corresponding candidate is no longer a leader.

-type leader() :: {winner(), certificate()}.
%% A candidate which wins the electoin and is certified as the leader.

-type elect_option() :: {priority, term()}
                      | {link, boolean()}
                      | find_option().
%% priority:
%% - The priority of the candidate.
%% - The smaller value means a higher priority.
%% - If conflict arises between multiple candidates in the same election, the highest priority one is eventually elected.
%% - The default value is `erlang:system_time(micro_seconds)'.
%%
%% link:
%% - If the value is `true', the candidate process and the certificate process will be linked.
%% - The default value is `true'.

-type find_option() :: {timeout, timeout()}
                     | {voter_count, pos_integer()}.
%% timeout:
%% - If some voter do not respond in the period, their votes are ignored.
%% - The default value is `100'.
%%
%% voter_count:
%% - The number of voters which vote for the election.
%% - The larger value is more tolerant to node failures and cluster member changes but more overhead occurs.
%% - Usually, but it is not mandatory, the same value in both {@link elect/2} and {@link find_leader/2} will be specified in the same election.
%% - The default value is `5'.

-type dismiss_option() :: {unlink, boolean()}
                        | {async, boolean()}.
%% unlink:
%% - If there is one, it removes the link between the candidate and the corresponding certificate process.
%% - Thus, the candidate process can survive after the dismissal.
%% - The default value is `false'.
%%
%% async:
%% - If the value is `true', the dismissal is processed by an asynchronous manner.
%% - The default value is `false'.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv elect(ElectionId, Candidate, [])
-spec elect(election_id(), candidate()) -> leader().
elect(ElectionId, Candidate) ->
    elect(ElectionId, Candidate, []).

%% @doc Elects the leader in the election
%%
%% If a leader have already been elected, it returns the leader.
%%
%% If conflict arises between multiple candidates in the same election,
%% the highest priority one is eventually elected (i.e., the leader is agreed by all member of the same erlang cluster).
%%
%% Point to notice is that temporary coexist of multiple leaders is not prohibited.
%% Some leaders will be eventually dismissed except highest priority one.
%%
%% If you are interested in the expiration of the term of office (or the dismissal) of a leader,
%% please you monitor the certificate process (i.e., `monitor(process, evel:get_certificate(Leader))').
%% The down of the certificate process indicates the retirement of the leader.
-passage_trace([{tags, #{election_id => "ElectionId", candidate => "Candidate"}}]).
-spec elect(election_id(), candidate(), [elect_option()]) -> Leader :: leader().
elect(ElectionId, Candidate, Options) ->
    _ = is_pid(Candidate) orelse error(badarg, [ElectionId, Candidate, Options]),
    _ = is_list(Options) orelse error(badarg, [ElectionId, Candidate, Options]),
    evel_commission:elect(ElectionId, Candidate, Options).

%% @equiv dismiss(Leader, [])
-spec dismiss(leader()) -> ok.
dismiss(Leader) ->
    dismiss(Leader, []).

%% @doc Dismisses the leader
%%
%% It kills (i.e., `exit(Pid, kill)') the corresponding certificate process.
%% As a result, the candidate process may exit if it have linked to the certificate process.
-passage_trace([{tags, #{leader => "Leader"}}]).
-spec dismiss(leader(), [dismiss_option()]) -> ok.
dismiss(Leader, Options) ->
    _ = is_leader(Leader) orelse error(badarg, [Leader, Options]),
    _ = is_list(Options) orelse error(badarg, [Leader, Options]),
    _ = case proplists:get_value(unlink, Options, false) of
            false -> ok;
            true  -> catch evel_agent:unlink_candidate(get_certificate(Leader))
        end,
    Async = proplists:get_value(async, Options, false),
    evel_commission:dismiss(Leader, Async).

%% @equiv find_leader(ElectionId, [])
-spec find_leader(election_id()) -> {ok, leader()} | error.
find_leader(ElectionId) ->
    find_leader(ElectionId, []).

%% @doc Finds the leader elected in the election
%%
%% If own node have already known the leader, this function will retrieve it from local ETS.
%% Otherwise it will try fetching the election result from remote nodes.
-passage_trace([{tags, #{election_id => "ElectionId"}}]).
-spec find_leader(election_id(), [find_option()]) -> {ok, leader()} | error.
find_leader(ElectionId, Options) ->
    _ = is_list(Options) orelse error(badarg, [ElectionId, Options]),
    evel_commission:find_leader(ElectionId, Options).

%% @doc Returns a list of locally known leaders
-passage_trace([]).
-spec known_leaders() -> [{election_id(), leader()}].
known_leaders() ->
    evel_commission:known_leaders().

%% @doc Gets the winner part of `Leader'
-spec get_winner(leader()) -> winner().
get_winner(Leader) ->
    _ = is_leader(Leader) orelse error(badarg, [Leader]),
    element(1, Leader).

%% @doc Gets the certificate part of `Leader'
-spec get_certificate(leader()) -> certificate().
get_certificate(Leader) ->
    _ = is_leader(Leader) orelse error(badarg, [leader]),
    element(2, Leader).

%% @doc Returns `true' if `X' is a `leader()', otherwise `false'
-spec is_leader(X :: (leader() | term())) -> boolean().
is_leader({Winner, Certificate}) -> is_pid(Winner) andalso is_pid(Certificate);
is_leader(_)                     -> false.
